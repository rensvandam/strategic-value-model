# Script to calculate domestic flows using CPB methodology
# Uses export shares from IO table applied to BACI exports
# Based on CPB gravity estimation paper methodology
# Author: Created for gravity model project
# Date: 2025

# Load required packages
library(tidyverse)
library(here)

# Configuration ================================================================
YEARS <- 2002:2022  # Years to process
DATA_PATH <- "data/baci-agg"
IO_TABLE_PATH <- "data/IOT/NATIOdomimp.rdata"
COUNTRY_CODES_PATH <- "data/baci/country_codes_V202501.csv"
OUTPUT_PATH <- DATA_PATH  # Save in same directory

# NOTE: BACI data is in thousands USD, IO table is in millions USD
# Script converts BACI to millions for consistency

# ROW country code
ROW_CODE <- 9000

# Manufacturing D-codes to extract from IO table (D10-D32)
MANUFACTURING_CODES <- c(
  "D10T12", "D13T15", "D16", "D17T18", "D19", "D20", "D21", "D22", "D23",
  "D24A", "D24B", "D25", "D26", "D27", "D28", "D29", "D301", "D302T309",
  "D31T33"
)

# Mapping of disaggregated D-codes in BACI to aggregated codes for final output
D_CODE_MAPPING <- tibble(
  original = c(
    "D10", "D11", "D12",           # Food, beverages, tobacco → D10T12
    "D13", "D14", "D15",           # Textiles, apparel, leather → D13T15
    "D16",                         # Wood products (not aggregated)
    "D17", "D18",                  # Paper and printing → D17T18
    "D19",                         # Coke and petroleum (not aggregated)
    "D20",                         # Chemicals (not aggregated)
    "D21",                         # Pharmaceuticals (not aggregated)
    "D22",                         # Rubber and plastics (not aggregated)
    "D23",                         # Non-metallic minerals (not aggregated)
    "D24",                         # Basic metals (not split in BACI)
    "D25",                         # Fabricated metal products
    "D26",                         # Computer, electronic, optical
    "D27",                         # Electrical equipment
    "D28",                         # Machinery and equipment
    "D29",                         # Motor vehicles
    "D30",                         # Other transport equipment
    "D31", "D32", "D33"            # Furniture and other manufacturing → D31T33
  ),
  aggregated = c(
    "D10T12", "D10T12", "D10T12",
    "D13T15", "D13T15", "D13T15",
    "D16",
    "D17T18", "D17T18",
    "D19",
    "D20",
    "D21",
    "D22",
    "D23",
    "D24",
    "D25",
    "D26",
    "D27",
    "D28",
    "D29",
    "D30",
    "D31T33", "D31T33", "D31T33"
  )
)

# Mapping of IO table D-codes to final output codes
# This aggregates D24A+D24B→D24 and D301+D302T309→D30
IO_TO_FINAL_MAPPING <- tibble(
  io_code = c(
    "D10T12", "D13T15", "D16", "D17T18", "D19", "D20", "D21", "D22", "D23",
    "D24A", "D24B", "D25", "D26", "D27", "D28", "D29", "D301", "D302T309",
    "D31T33"
  ),
  final_code = c(
    "D10T12", "D13T15", "D16", "D17T18", "D19", "D20", "D21", "D22", "D23",
    "D24", "D24", "D25", "D26", "D27", "D28", "D29", "D30", "D30",
    "D31T33"
  )
)

# Helper Functions =============================================================

#' Load IO table from RData format
load_io_table <- function(path) {
  
  if (!file.exists(path)) {
    stop("IO table not found at: ", path)
  }
  
  cat("Loading RData file...\n")
  temp_env <- new.env()
  load(path, envir = temp_env)
  
  loaded_objects <- ls(temp_env)
  cat("Objects in RData file:", paste(loaded_objects, collapse = ", "), "\n")
  
  if ("NATIOdomimp" %in% loaded_objects) {
    io_table <- get("NATIOdomimp", envir = temp_env)
    cat("Found IO table object: NATIOdomimp\n")
    return(io_table)
  }
  
  for (obj_name in loaded_objects) {
    obj <- get(obj_name, envir = temp_env)
    if (is.array(obj) && length(dim(obj)) == 4) {
      cat("Found IO table object:", obj_name, "\n")
      return(obj)
    }
  }
  
  stop("Could not find a 4-dimensional array in the RData file.\n",
       "Available objects: ", paste(loaded_objects, collapse = ", "))
}

#' Read and process country code mapping
read_country_mapping <- function(path) {
  country_codes <- read_csv(path, show_col_types = FALSE)
  
  required_cols <- c("country_code", "country_iso3")
  if (!all(required_cols %in% names(country_codes))) {
    stop("Country codes file must contain: ", paste(required_cols, collapse = ", "))
  }
  
  return(country_codes)
}

#' Aggregate ROW regions (9001-9005) to single ROW (9000)
aggregate_row_regions <- function(df) {
  df %>%
    mutate(
      i = if_else(i %in% 9001:9005, ROW_CODE, i),
      j = if_else(j %in% 9001:9005, ROW_CODE, j)
    ) %>%
    group_by(i, j, k, t) %>%
    summarise(v = sum(v, na.rm = TRUE), .groups = "drop")
}

#' Map disaggregated D-codes to aggregated codes
aggregate_d_codes <- function(df, mapping) {
  df %>%
    left_join(mapping, by = c("k" = "original")) %>%
    mutate(k = coalesce(aggregated, k)) %>%
    select(-aggregated) %>%
    group_by(i, j, k, t) %>%
    summarise(v = sum(v, na.rm = TRUE), .groups = "drop")
}

#' Aggregate IO table codes to final output codes
aggregate_io_codes <- function(df, mapping) {
  df %>%
    left_join(mapping, by = c("k" = "io_code")) %>%
    mutate(k = coalesce(final_code, k)) %>%
    select(-final_code) %>%
    group_by(country_iso3, k, t) %>%
    summarise(
      output = sum(output, na.rm = TRUE),
      exports_io = sum(exports_io, na.rm = TRUE),
      .groups = "drop"
    )
}

#' Match countries between BACI and IO table
match_countries <- function(baci_countries, io_countries, country_mapping) {
  
  baci_with_iso <- baci_countries %>%
    left_join(
      country_mapping %>% select(country_code, country_iso3),
      by = "country_code"
    )
  
  matches <- baci_with_iso %>%
    filter(country_iso3 %in% io_countries) %>%
    select(country_code, country_iso3)
  
  not_in_io <- baci_with_iso %>%
    filter(!country_iso3 %in% io_countries | is.na(country_iso3))
  
  io_to_row <- io_countries[!io_countries %in% matches$country_iso3 & io_countries != "ROW"]
  
  cat("\n=== Country Matching Results ===\n")
  cat("Countries matched:", nrow(matches), "\n")
  cat("BACI countries excluded (not in IO):", nrow(not_in_io), "\n")
  cat("IO countries aggregated to ROW:", length(io_to_row), "\n\n")
  
  return(list(
    matches = matches,
    excluded = not_in_io$country_code,
    io_to_row = io_to_row
  ))
}

#' Extract OUTPUT and EXPO from IO table - CORRECTED
extract_io_data <- function(io_table, year, countries_iso3, d_codes) {
  
  year_idx <- which(dimnames(io_table)[[1]] == as.character(year))
  if (length(year_idx) == 0) {
    stop("Year ", year, " not found in IO table")
  }
  
  # For each industry code, we need:
  # OUTPUT: io_table[year, country, "OUTPUT", industry_code]
  # EXPORTS: io_table[year, country, "DOM_industry_code", "EXPO"]
  
  results <- list()
  
  for (k_code in d_codes) {
    # Industry code for OUTPUT dimension (e.g., "D10T12")
    output_col <- k_code
    
    # Industry code for EXPORTS dimension (e.g., "DOM_10T12")
    # Remove the 'D' prefix: "D10T12" -> "10T12"
    industry_suffix <- sub("^D", "", k_code)
    export_row <- paste0("DOM_", industry_suffix)
    
    # Check if both exist
    if (!(output_col %in% dimnames(io_table)[[4]])) {
      cat("Warning: Industry", k_code, "not found in OUTPUT dimension\n")
      next
    }
    
    if (!(export_row %in% dimnames(io_table)[[3]])) {
      cat("Warning: Industry", export_row, "not found in DOM dimension\n")
      next
    }
    
    # Extract for all countries
    for (country in countries_iso3) {
      if (!(country %in% dimnames(io_table)[[2]])) next
      
      output_val <- io_table[year_idx, country, "OUTPUT", output_col]
      export_val <- io_table[year_idx, country, export_row, "EXPO"]
      
      results[[length(results) + 1]] <- tibble(
        country_iso3 = country,
        k = k_code,
        t = year,
        output = output_val,
        exports_io = export_val
      )
    }
  }
  
  # Combine all results
  io_long <- bind_rows(results)
  
  return(io_long)
}

#' Calculate total exports by country-industry-year from BACI
calculate_exports <- function(trade_data) {
  trade_data %>%
    group_by(i, k, t) %>%
    summarise(exports_baci = sum(v, na.rm = TRUE), .groups = "drop")
}

#' Calculate domestic flows using CPB methodology
#' Export share from IO × BACI exports → Implied output → Domestic
calculate_domestic_flows_cpb <- function(io_data, exports_baci, country_mapping) {
  
  cat("\n=== CPB Methodology: Export Shares ===\n")
  
  # Join country codes
  domestic <- io_data %>%
    left_join(
      country_mapping %>% select(country_code, country_iso3),
      by = "country_iso3"
    ) %>%
    mutate(country_code = if_else(country_iso3 == "ROW", ROW_CODE, country_code))
  
  # Calculate export share from IO table BY INDUSTRY
  # Export share = exports_io / output (for each country-industry)
  domestic <- domestic %>%
    mutate(
      export_share = if_else(output > 0, exports_io / output, 0),
      export_share = pmax(export_share, 0.0001)  # Avoid division by zero, min 0.01%
    )
  
  cat("Export shares calculated by industry\n")
  cat("  Mean export share:", round(mean(domestic$export_share, na.rm = TRUE) * 100, 2), "%\n")
  cat("  Median export share:", round(median(domestic$export_share, na.rm = TRUE) * 100, 2), "%\n")
  cat("  Min export share:", round(min(domestic$export_share, na.rm = TRUE) * 100, 4), "%\n")
  cat("  Max export share:", round(max(domestic$export_share, na.rm = TRUE) * 100, 2), "%\n")
  
  # Join with BACI exports
  domestic <- domestic %>%
    left_join(
      exports_baci,
      by = c("country_code" = "i", "k", "t")
    ) %>%
    mutate(exports_baci = coalesce(exports_baci, 0))
  
  # CPB method: 
  # 1. Export share from IO = exports_io / output_io
  # 2. Implied output = exports_baci / export_share
  # 3. Domestic = Implied output - exports_baci
  domestic <- domestic %>%
    mutate(
      implied_output = exports_baci / export_share,
      domestic_flow = implied_output - exports_baci,
      domestic_flow = pmax(domestic_flow, 0)  # Can't be negative
    )
  
  # Report on cases where export share > 1 (exports exceed output in IO)
  high_share <- domestic %>%
    filter(export_share > 1)
  
  if (nrow(high_share) > 0) {
    cat("\nWARNING:", nrow(high_share), "observations where IO exports > IO output\n")
    cat("This indicates re-export issues in IO table itself\n")
    cat("Top cases:\n")
    print(high_share %>%
            select(country_iso3, k, output, exports_io, export_share) %>%
            arrange(desc(export_share)) %>%
            head(5))
  }
  
  # Create final domestic flow data
  result <- domestic %>%
    mutate(
      i = country_code,
      j = country_code,
      v = domestic_flow
    ) %>%
    select(i, j, k, t, v)
  
  cat("\nDomestic flows summary:\n")
  cat("  Total domestic flow:", round(sum(result$v, na.rm = TRUE), 0), "million USD\n")
  cat("  Mean domestic flow:", round(mean(result$v, na.rm = TRUE), 0), "million USD\n")
  cat("  Zero domestic flows:", sum(result$v == 0), "observations\n")
  
  return(result)
}

#' Process one year
process_year <- function(year, io_table, country_mapping, country_matches) {
  
  cat("\n=== Processing year", year, "===\n")
  
  input_file <- file.path(DATA_PATH, paste0("BACI_HS02_Y", year, "_V202501.csv"))
  
  if (!file.exists(input_file)) {
    cat("File not found:", input_file, "\n")
    return(NULL)
  }
  
  cat("Reading:", input_file, "\n")
  trade_data <- read_csv(input_file, show_col_types = FALSE)
  
  # Process trade data
  trade_data <- trade_data %>% 
    select(i, j, k, t, v) %>%
    mutate(v = v / 1000)  # Convert thousands to millions
  
  cat("Original data:", nrow(trade_data), "rows\n")
  
  trade_data <- aggregate_row_regions(trade_data)
  trade_data <- aggregate_d_codes(trade_data, D_CODE_MAPPING)
  trade_data <- trade_data %>%
    filter(
      i %in% c(country_matches$matches$country_code, ROW_CODE),
      j %in% c(country_matches$matches$country_code, ROW_CODE)
    )
  
  cat("After processing:", nrow(trade_data), "rows\n")
  
  # Extract IO data
  cat("Extracting IO data...\n")
  all_iso3 <- c(country_matches$matches$country_iso3, country_matches$io_to_row)
  
  io_data <- extract_io_data(io_table, year, all_iso3, MANUFACTURING_CODES)
  
  # Aggregate IO countries to ROW
  io_data <- io_data %>%
    mutate(
      country_iso3 = if_else(country_iso3 %in% country_matches$io_to_row, 
                             "ROW", country_iso3)
    ) %>%
    group_by(country_iso3, k, t) %>%
    summarise(
      output = sum(output, na.rm = TRUE),
      exports_io = sum(exports_io, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Aggregate IO codes
  io_data <- aggregate_io_codes(io_data, IO_TO_FINAL_MAPPING)
  
  cat("IO data extracted:", nrow(io_data), "observations\n")
  
  # Calculate exports from BACI
  exports_baci <- calculate_exports(trade_data)
  cat("BACI exports calculated:", nrow(exports_baci), "observations\n")
  
  # Calculate domestic flows using CPB method
  domestic <- calculate_domestic_flows_cpb(io_data, exports_baci, country_mapping)
  cat("Domestic flows calculated:", nrow(domestic), "observations\n")
  
  # Combine
  gravity_ready <- bind_rows(trade_data, domestic) %>%
    arrange(i, j, k, t)
  
  cat("Final dataset:", nrow(gravity_ready), "rows\n")
  
  # Save
  backup_file <- file.path(DATA_PATH, paste0("BACI_HS02_Y", year, "_V202501_BACKUP.csv"))
  output_file <- file.path(DATA_PATH, paste0("BACI_HS02_Y", year, "_V202501_gravityready.csv"))
  
  if (!file.exists(backup_file)) {
    file.copy(input_file, backup_file)
  }
  
  write_csv(gravity_ready, output_file)
  cat("Saved to:", output_file, "\n")
  
  return(gravity_ready)
}

# Main =========================================================================

main <- function() {
  
  cat("========================================\n")
  cat("GRAVITY-READY DATA PREPARATION\n")
  cat("CPB Methodology (Export Shares)\n")
  cat("========================================\n\n")
  
  io_table <- load_io_table(IO_TABLE_PATH)
  
  cat("\nIO table dimensions:\n")
  cat("  Years:", length(dimnames(io_table)[[1]]), "\n")
  cat("  Countries:", length(dimnames(io_table)[[2]]), "\n")
  cat("  Categories:", length(dimnames(io_table)[[3]]), "\n")
  cat("  Industries:", length(dimnames(io_table)[[4]]), "\n")
  cat("  Dim 4 names (first 10):", paste(head(dimnames(io_table)[[4]], 10), collapse = ", "), "\n")
  cat("  Dim 4 names (last 10):", paste(tail(dimnames(io_table)[[4]], 10), collapse = ", "), "\n")
  
  country_mapping <- read_country_mapping(COUNTRY_CODES_PATH)
  
  first_year_file <- file.path(DATA_PATH, paste0("BACI_HS02_Y", YEARS[1], "_V202501.csv"))
  baci_sample <- read_csv(first_year_file, show_col_types = FALSE)
  baci_countries <- tibble(country_code = unique(c(baci_sample$i, baci_sample$j)))
  
  country_matches <- match_countries(baci_countries, dimnames(io_table)[[2]], country_mapping)
  
  results <- list()
  for (year in YEARS) {
    tryCatch({
      results[[as.character(year)]] <- process_year(year, io_table, country_mapping, country_matches)
    }, error = function(e) {
      cat("\n!!! ERROR processing year", year, "!!!\n")
      cat("Error message:", e$message, "\n\n")
    })
  }
  
  cat("\n========================================\n")
  cat("PROCESSING COMPLETE\n")
  cat("Years processed:", length(results), "out of", length(YEARS), "\n")
  cat("========================================\n")
  
  return(invisible(results))
}

if (interactive()) {
  cat("Script loaded. Run main() to execute.\n")
} else {
  main()
}
