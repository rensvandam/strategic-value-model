# Script to process bilateral WITS tariff data
# Handles multi-year combined files, processes year by year
# Outputs separate file per year
# Author: Created for gravity model project  
# Date: 2025

library(tidyverse)
library(readxl)
library(data.table)  # For faster reading of large files

# Configuration ================================================================
BILATERAL_TARIFF_PATH <- "data/tariffs/bilateral"
CONCORDANCE_BTDI <- "data/concordances/BTDIxEConvKey.xlsx"
CONCORDANCE_H6_H5 <- "data/concordances/Concordance_H6_to_H5.CSV"
INDUSTRY_CODES_FILE <- "data/concordances/industry_codes.csv"
OUTPUT_PATH <- "data/tariffs/processed"

# Columns to keep from WITS bilateral data
COLS_TO_KEEP <- c(
  "Selected Nomen",
  "Reporter", 
  "Partner",
  "Product",
  "Tariff Year",
  "DutyType",
  "Simple Average"
)

# Helper Functions =============================================================

#' Get unique years from a tariff file (reads only the year column)
#' Uses data.table for speed
get_unique_years <- function(file_path) {
  cat("Scanning for years in file...\n")
  
  # Read only the Tariff Year column using data.table (fast)
  dt <- fread(
    file_path, 
    select = "Tariff Year",
    showProgress = FALSE
  )
  
  years <- sort(unique(dt$`Tariff Year`))
  cat("  Found years:", paste(years, collapse = ", "), "\n\n")
  
  return(years)
}

#' Read tariff file filtered to a specific year
#' Uses data.table for speed and memory efficiency
read_tariff_year <- function(file_path, target_year) {
  cat(sprintf("Reading data for year %d...\n", target_year))
  
  # Read full file with data.table (faster than read_csv)
  dt <- fread(file_path, showProgress = FALSE)
  
  # Filter to target year immediately
  dt <- dt[`Tariff Year` == target_year]
  
  cat(sprintf("  Rows for year %d: %d\n", target_year, nrow(dt)))
  
  # Convert to tibble for tidyverse compatibility
  return(as_tibble(dt))
}

#' Process tariff data for a single year
#' Takes already-filtered data (not file path)
process_tariff_year <- function(
    tariff_raw,
    target_year,
    h6_h5_concordance,
    btdi_concordance,
    industry_mapping
) {
  
  cat(sprintf("\n--- Processing year %d ---\n", target_year))
  
  # Check required columns
  required_cols <- c("Reporter", "Partner", "Product", "Tariff Year", "Simple Average")
  missing <- setdiff(required_cols, names(tariff_raw))
  
  if (length(missing) > 0) {
    stop("Missing required columns: ", paste(missing, collapse = ", "))
  }
  
  # Clean and filter
  cat("Cleaning data...\n")
  
  tariff_clean <- tariff_raw %>%
    # Keep only needed columns
    select(any_of(COLS_TO_KEEP)) %>%
    # Standardize names
    rename(
      reporter = Reporter,
      partner = Partner,
      product = Product,
      year = "Tariff Year",
      tariff = "Simple Average"
    ) %>%
    # Handle optional columns
    mutate(
      hs_nomen = if ("Selected Nomen" %in% names(.)) `Selected Nomen` else NA_character_,
      duty_type = if ("DutyType" %in% names(.)) DutyType else "AHS",
      product = str_pad(as.character(product), 6, pad = "0")
    ) %>%
    # Filter
    filter(
      !is.na(tariff),
      tariff >= 0,
      tariff < 999  # Remove outliers/errors
    )
  
  cat("  After cleaning:", nrow(tariff_clean), "rows\n")
  
  if (nrow(tariff_clean) == 0) {
    cat("  WARNING: No rows after cleaning. Skipping year.\n")
    return(NULL)
  }
  
  # Parse HS version
  cat("Parsing HS version...\n")
  
  tariff_clean <- tariff_clean %>%
    mutate(
      hs_version = case_when(
        str_detect(hs_nomen, "H0|1988|1992") ~ 0L,
        str_detect(hs_nomen, "H1|1996") ~ 1L,
        str_detect(hs_nomen, "H2|2002") ~ 2L,
        str_detect(hs_nomen, "H3|2007") ~ 3L,
        str_detect(hs_nomen, "H4|2012") ~ 4L,
        str_detect(hs_nomen, "H5|2017") ~ 5L,
        str_detect(hs_nomen, "H6|2022") ~ 6L,
        is.na(hs_nomen) ~ guess_hs_version(target_year),
        TRUE ~ guess_hs_version(target_year)
      )
    )
  
  hs_versions <- unique(tariff_clean$hs_version)
  cat("  HS versions in data:", paste(hs_versions, collapse = ", "), "\n")
  
  # Process each HS version separately
  results_list <- list()
  
  for (hs_ver in hs_versions) {
    cat(sprintf("\n  Processing HS version %d...\n", hs_ver))
    
    tariff_hs <- tariff_clean %>%
      filter(hs_version == hs_ver)
    
    cat("    Rows:", nrow(tariff_hs), "\n")
    
    # Convert H6 to H5 if needed
    if (hs_ver %in% c(5, 6)) {
      cat("    Converting to HS 2017...\n")
      
      if (hs_ver == 5) {
        tariff_hs <- tariff_hs %>%
          left_join(
            h6_h5_concordance %>% 
              select(product = hs2017, hs5 = hs2017) %>% 
              distinct(),
            by = "product"
          )
      } else {
        tariff_hs <- tariff_hs %>%
          left_join(
            h6_h5_concordance %>% 
              select(product = hs2022, hs5 = hs2017) %>% 
              distinct(),
            by = "product"
          )
      }
      
      tariff_hs <- tariff_hs %>%
        filter(!is.na(hs5)) %>%
        mutate(product = hs5) %>%
        select(-hs5)
      
      cat("    After H6->H5 conversion:", nrow(tariff_hs), "rows\n")
    }
    
    # Map to ISIC
    cat("    Mapping to ISIC...\n")
    
    concordance_ver <- ifelse(hs_ver >= 5, 5L, hs_ver)
    
    btdi_filtered <- btdi_concordance %>%
      filter(hs_version == concordance_ver)
    
    tariff_isic <- tariff_hs %>%
      left_join(
        btdi_filtered %>% select(product = hs6, isic),
        by = "product"
      ) %>%
      filter(!is.na(isic))
    
    cat("    Mapped to ISIC:", nrow(tariff_isic), "rows\n")
    
    if (nrow(tariff_isic) == 0) {
      cat("    WARNING: No rows mapped to ISIC. Skipping HS version.\n")
      next
    }
    
    # Filter to manufacturing
    cat("    Filtering to manufacturing...\n")
    
    # Check ISIC format (with or without "C" prefix)
    sample_isic <- head(unique(tariff_isic$isic), 5)
    has_c_prefix <- any(str_starts(sample_isic, "C"))
    
    if (has_c_prefix) {
      tariff_manuf <- tariff_isic %>%
        filter(
          str_starts(isic, "C1") | str_starts(isic, "C2") | str_starts(isic, "C3")
        ) %>%
        filter(isic >= "C10", isic <= "C33")
    } else {
      tariff_manuf <- tariff_isic %>%
        mutate(isic_num = as.integer(str_extract(isic, "\\d+"))) %>%
        filter(
          !is.na(isic_num),
          isic_num >= 10,
          isic_num <= 33
        ) %>%
        mutate(isic = paste0("C", isic_num)) %>%
        select(-isic_num)
    }
    
    cat("    Manufacturing only:", nrow(tariff_manuf), "rows\n")
    
    if (nrow(tariff_manuf) == 0) {
      cat("    WARNING: No manufacturing rows found.\n")
      next
    }
    
    # Aggregate to ISIC level
    tariff_agg_isic <- tariff_manuf %>%
      group_by(reporter, partner, year, isic) %>%
      summarise(
        tariff = mean(tariff, na.rm = TRUE),
        n_products = n(),
        .groups = "drop"
      )
    
    # Map to D-codes
    tariff_dcodes <- tariff_agg_isic %>%
      left_join(industry_mapping, by = "isic") %>%
      filter(!is.na(d_code))
    
    # Aggregate to D-codes
    tariff_final <- tariff_dcodes %>%
      group_by(reporter, partner, year, d_code) %>%
      summarise(
        tariff = mean(tariff, na.rm = TRUE),
        n_isic_codes = n(),
        n_products = sum(n_products),
        .groups = "drop"
      )
    
    cat("    Final D-codes:", nrow(tariff_final), "rows\n")
    
    results_list[[as.character(hs_ver)]] <- tariff_final
  }
  
  # Combine all HS versions
  if (length(results_list) == 0) {
    cat("  WARNING: No data processed for year", target_year, "\n")
    return(NULL)
  }
  
  tariff_combined <- bind_rows(results_list)
  
  # Final aggregation in case same reporter-partner-industry appeared in multiple HS versions
  tariff_combined <- tariff_combined %>%
    group_by(reporter, partner, year, d_code) %>%
    summarise(
      tariff = mean(tariff, na.rm = TRUE),
      n_isic_codes = sum(n_isic_codes),
      n_products = sum(n_products),
      .groups = "drop"
    )
  
  cat(sprintf("✓ Year %d: %d rows processed\n", target_year, nrow(tariff_combined)))
  
  
  return(tariff_combined)
}

#' Guess HS version based on year
guess_hs_version <- function(year) {
  case_when(
    year <= 1995 ~ 0L,
    year <= 2001 ~ 1L,
    year <= 2006 ~ 2L,
    year <= 2011 ~ 3L,
    year <= 2016 ~ 4L,
    year <= 2021 ~ 5L,
    TRUE ~ 6L
  )
}

#' Load concordances
load_concordances <- function() {
  
  cat("Loading concordances...\n")
  
  # H6-H5
  cat("  Loading H6-H5 concordance...\n")
  h6_h5 <- read_csv(CONCORDANCE_H6_H5, show_col_types = FALSE) %>%
    select(
      hs2017 = `HS 2017 Product Code`,
      hs2022 = `HS 2022 Product Code`
    ) %>%
    mutate(
      hs2017 = str_pad(as.character(hs2017), 6, pad = "0"),
      hs2022 = str_pad(as.character(hs2022), 6, pad = "0")
    )
  cat("    Rows:", nrow(h6_h5), "\n")
  
  # BTDIxE
  cat("  Loading BTDIxE concordance...\n")
  btdi <- read_excel(CONCORDANCE_BTDI, sheet = "FromHSToISICToEC") %>%
    select(
      hs_version = 'HS',
      hs6 = 'product',
      isic = 'Desci4'
    ) %>%
    mutate(
      hs6 = str_pad(as.character(hs6), 6, pad = "0"),
      isic = as.character(isic),
      hs_version = as.integer(hs_version)
    )
  cat("    Rows:", nrow(btdi), "\n")
  
  # Industry mapping
  cat("  Loading industry mapping...\n")
  if (file.exists(INDUSTRY_CODES_FILE)) {
    industry <- read_csv(INDUSTRY_CODES_FILE, show_col_types = FALSE) %>%
      rename(isic = 1, d_code = 2)
  } else {
    cat("    Using default industry mapping\n")
    industry <- tibble(
      isic = c(
        "C10", "C11", "C12",
        "C13", "C14", "C15",
        "C16", "C17", "C18", "C19", "C20", "C21", "C22", "C23",
        "C24", "C24A", "C24B",
        "C25", "C26", "C27", "C28", "C29",
        "C30", "C301", "C302", "C303",
        "C31", "C32", "C33"
      ),
      d_code = c(
        "D10T12", "D10T12", "D10T12",
        "D13T15", "D13T15", "D13T15",
        "D16", "D17T18", "D17T18",
        "D19", "D20", "D21", "D22", "D23",
        "D24", "D24", "D24",
        "D25", "D26", "D27", "D28", "D29",
        "D30", "D30", "D30", "D30",
        "D31T33", "D31T33", "D31T33"
      )
    )
  }
  cat("    Industries:", n_distinct(industry$d_code), "\n")
  
  cat("✓ Concordances loaded\n\n")
  
  return(list(
    h6_h5 = h6_h5,
    btdi = btdi,
    industry = industry
  ))
}

#' Print summary statistics for processed data
print_summary <- function(tariff_processed, year) {
  cat(sprintf("\nSummary for year %d:\n", year))
  cat("  Mean tariff:", round(mean(tariff_processed$tariff), 2), "%\n")
  cat("  Median tariff:", round(median(tariff_processed$tariff), 2), "%\n")
  cat("  Reporters:", n_distinct(tariff_processed$reporter), "\n")
  cat("  Partners:", n_distinct(tariff_processed$partner), "\n")
  cat("  Observations:", nrow(tariff_processed), "\n")
}

# Main Functions ===============================================================

#' Process a single file that may contain multiple years
#' Saves separate output file for each year
process_file <- function(file_path, concordances = NULL) {
  
  cat("\n========================================\n")
  cat("PROCESSING FILE\n")
  cat(basename(file_path), "\n")
  cat("========================================\n\n")
  
  # Load concordances if not provided
  if (is.null(concordances)) {
    concordances <- load_concordances()
  }
  
  # Create output directory
  if (!dir.exists(OUTPUT_PATH)) {
    dir.create(OUTPUT_PATH, recursive = TRUE)
  }
  
  # Get all years in this file
  years <- get_unique_years(file_path)
  
  # Process each year
  results_summary <- list()
  
  for (yr in years) {
    
    # Read data for this year only
    tariff_raw <- read_tariff_year(file_path, yr)
    
    if (nrow(tariff_raw) == 0) {
      cat(sprintf("  No data for year %d, skipping.\n", yr))
      next
    }
    
    # Process
    tariff_processed <- process_tariff_year(
      tariff_raw,
      yr,
      concordances$h6_h5,
      concordances$btdi,
      concordances$industry
    )
    
    # Clean up to free memory
    rm(tariff_raw)
    gc()
    
    if (is.null(tariff_processed) || nrow(tariff_processed) == 0) {
      cat(sprintf("  No output for year %d.\n", yr))
      next
    }
    
    # Print summary
    print_summary(tariff_processed, yr)
    
    # Save
    output_file <- file.path(OUTPUT_PATH, sprintf("tariffs_bilateral_%d.csv", yr))
    write_csv(tariff_processed, output_file)
    cat(sprintf("✓ Saved: %s\n", output_file))
    
    # Store summary
    results_summary[[as.character(yr)]] <- tibble(
      year = yr,
      n_rows = nrow(tariff_processed),
      n_reporters = n_distinct(tariff_processed$reporter),
      n_partners = n_distinct(tariff_processed$partner),
      mean_tariff = mean(tariff_processed$tariff)
    )
    
    # Clean up
    rm(tariff_processed)
    gc()
  }
  
  # Print overall summary
  cat("\n========================================\n")
  cat("FILE PROCESSING COMPLETE\n")
  cat("========================================\n\n")
  
  if (length(results_summary) > 0) {
    summary_df <- bind_rows(results_summary)
    print(summary_df)
    return(summary_df)
  } else {
    cat("No data processed from this file.\n")
    return(NULL)
  }
}

#' Process all CSV files in a directory
process_all_files <- function(directory = BILATERAL_TARIFF_PATH) {
  
  cat("\n########################################\n")
  cat("BATCH PROCESSING ALL TARIFF FILES\n")
  cat("########################################\n\n")
  
  # Load concordances once
  concordances <- load_concordances()
  
  # Find all CSV files
  files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE)
  
  cat("Found", length(files), "CSV files:\n")
  for (f in files) {
    cat("  -", basename(f), "\n")
  }
  cat("\n")
  
  # Process each file
  all_summaries <- list()
  
  for (file_path in files) {
    summary <- process_file(file_path, concordances)
    if (!is.null(summary)) {
      all_summaries[[basename(file_path)]] <- summary
    }
  }
  
  # Final summary
  cat("\n########################################\n")
  cat("ALL FILES PROCESSED\n")
  cat("########################################\n\n")
  
  if (length(all_summaries) > 0) {
    final_summary <- bind_rows(all_summaries, .id = "source_file") %>%
      arrange(year)
    
    cat("Years processed:\n")
    print(final_summary %>% select(year, n_rows, mean_tariff))
    
    # Save summary
    summary_file <- file.path(OUTPUT_PATH, "processing_summary.csv")
    write_csv(final_summary, summary_file)
    cat("\n✓ Summary saved:", summary_file, "\n")
    
    return(final_summary)
  }
  
  return(NULL)
}

# Run ==========================================================================

if (interactive()) {
  cat("\n")
  cat("========================================\n")
  cat("BILATERAL TARIFF PROCESSING SCRIPT\n
")
  cat("========================================\n\n")
  cat("Usage:\n")
  cat("  1. Process single file:    process_file('path/to/file.csv')\n")
  cat("  2. Process all files:      process_all_files()\n")
  cat("  3. Process specific dir:   process_all_files('path/to/directory')\n")
  cat("\n")
  cat("Expected directory structure:\n")
  cat("  data/\n")
  cat("    concordances/\n")
  cat("      BTDIxEConvKey.xlsx\n")
  cat("      Concordance_H6_to_H5.CSV\n")
  cat("      industry_codes.csv (optional)\n")
  cat("    tariffs/\n")
  cat("      bilateral/\n")
  cat("        <your tariff CSV files>\n")
  cat("      processed/\n")
  cat("        <output will go here>\n")
  cat("\n")
} else {
  # Command line usage
  args <- commandArgs(trailingOnly = TRUE)
  
  if (length(args) == 0) {
    # No arguments: process all files in default directory
    process_all_files()
  } else if (length(args) == 1) {
    if (dir.exists(args[1])) {
      # Argument is a directory
      process_all_files(args[1])
    } else if (file.exists(args[1])) {
      # Argument is a file
      process_file(args[1])
    } else {
      stop("Path not found: ", args[1])
    }
  } else {
    # Multiple arguments: treat as list of files
    concordances <- load_concordances()
    for (f in args) {
      if (file.exists(f)) {
        process_file(f, concordances)
      } else {
        cat("File not found, skipping:", f, "\n")
      }
    }
  }
}
