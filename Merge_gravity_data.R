# Script to merge gravity variables with yearly BACI trade data
# Adds distance, language, colonial ties, trade agreements, etc.
# Author: Created for gravity model project
# Date: 2025

library(tidyverse)
library(here)

# Configuration ================================================================
GRAVITY_FILE <- "data/Gravity_V202211.rds"
TRADE_DATA_PATH <- "data/baci-agg"
COUNTRY_CODES_PATH <- "data/baci/country_codes_V202501.csv"
YEARS <- 2002:2022

# Select relevant gravity variables
GRAVITY_VARS <- c(
  # Identifiers
  "year", "iso3_o", "iso3_d",
  
  # Geographic
  "distw_harmonic",          # Population-weighted distance (harmonic mean) - BEST
  "dist",                    # Simple distance (backup)
  "distcap",                 # Capital-to-capital distance (backup)
  "contig",                  # Contiguous (shared border)
  
  # Cultural/Historical
  "comlang_off",             # Common official language
  "comlang_ethno",           # Common ethnographic language
  "comcol",                  # Common colonizer
  "col45",                   # Colonial relationship post-1945
  "col_dep_ever",            # Ever in colonial relationship
  "sibling_ever",            # Ever part of same country
  
  # Trade Policy
  "fta_wto",                 # Free trade agreement (WTO definition)
  "eu_o", "eu_d",            # EU membership (origin, destination)
  "wto_o", "wto_d",          # WTO membership (origin, destination)
  "gatt_o", "gatt_d",        # GATT membership (origin, destination)
  "rta_coverage",            # RTA coverage (0-7 scale)
  "rta_type",                # RTA type
  
  # Economic (optional - useful for diagnostics)
  "gdp_o", "gdp_d",          # GDP (origin, destination)
  "gdpcap_o", "gdpcap_d"     # GDP per capita
)

# Helper Functions =============================================================

#' Load gravity data
load_gravity_data <- function(path = GRAVITY_FILE) {
  
  cat("========================================\n")
  cat("LOADING GRAVITY DATA\n")
  cat("========================================\n\n")
  
  if (!file.exists(path)) {
    stop("Gravity file not found: ", path)
  }
  
  cat("Reading:", path, "\n")
  gravity <- readRDS(path)
  
  cat("Original dimensions:", nrow(gravity), "rows,", ncol(gravity), "columns\n")
  cat("Years covered:", paste(range(gravity$year), collapse = " to "), "\n")
  cat("Countries:", length(unique(c(gravity$iso3_o, gravity$iso3_d))), "\n")
  
  # Check which variables are available
  cat("\nChecking variable availability:\n")
  available <- GRAVITY_VARS[GRAVITY_VARS %in% names(gravity)]
  missing <- GRAVITY_VARS[!GRAVITY_VARS %in% names(gravity)]
  
  cat("  Available:", length(available), "of", length(GRAVITY_VARS), "\n")
  if (length(missing) > 0) {
    cat("  Missing variables:", paste(missing, collapse = ", "), "\n")
  }
  
  # Select available variables
  gravity_subset <- gravity %>%
    select(any_of(GRAVITY_VARS))
  
  cat("\nSubset dimensions:", nrow(gravity_subset), "rows,", ncol(gravity_subset), "columns\n")
  
  return(gravity_subset)
}

#' Load country code mapping
load_country_mapping <- function(path = COUNTRY_CODES_PATH) {
  
  cat("\n--- Loading Country Code Mapping ---\n")
  
  if (!file.exists(path)) {
    stop("Country codes file not found: ", path)
  }
  
  country_codes <- read_csv(path, show_col_types = FALSE)
  cat("Country codes loaded:", nrow(country_codes), "countries\n")
  
  return(country_codes)
}

#' Prepare gravity data for merge
prepare_gravity_for_merge <- function(gravity_data, country_mapping) {
  
  cat("\n--- Preparing Gravity Data for Merge ---\n")
  
  # Create mapping from iso3 to country_code
  iso_to_code <- country_mapping %>%
    select(country_code, country_iso3) %>%
    distinct()
  
  # Map origin countries
  gravity_prep <- gravity_data %>%
    left_join(
      iso_to_code %>% rename(i = country_code),
      by = c("iso3_o" = "country_iso3")
    ) %>%
    left_join(
      iso_to_code %>% rename(j = country_code),
      by = c("iso3_d" = "country_iso3")
    )
  
  # Check matching success
  matched_o <- sum(!is.na(gravity_prep$i))
  matched_d <- sum(!is.na(gravity_prep$j))
  total <- nrow(gravity_prep)
  
  cat("Origin countries matched:", matched_o, "of", total, 
      "(", round(100 * matched_o / total, 1), "%)\n")
  cat("Destination countries matched:", matched_d, "of", total,
      "(", round(100 * matched_d / total, 1), "%)\n")
  
  # Keep only matched pairs
  gravity_prep <- gravity_prep %>%
    filter(!is.na(i), !is.na(j)) %>%
    select(-iso3_o, -iso3_d)  # Remove iso3 codes, keep country_code
  
  cat("After filtering unmatched:", nrow(gravity_prep), "rows\n")
  
  # Rename year to t for consistency
  gravity_prep <- gravity_prep %>%
    rename(t = year)
  
  return(gravity_prep)
}

#' Handle 2022 data (use 2021 values)
extend_gravity_to_2022 <- function(gravity_data) {
  
  cat("\n--- Extending Gravity Data to 2022 ---\n")
  cat("Using 2021 values for 2022 (most variables don't change)\n")
  
  gravity_2021 <- gravity_data %>%
    filter(t == 2021) %>%
    mutate(t = 2022)
  
  cat("Created", nrow(gravity_2021), "observations for 2022\n")
  
  # Combine with original data
  gravity_extended <- bind_rows(
    gravity_data,
    gravity_2021
  ) %>%
    arrange(t, i, j)
  
  cat("Total after extension:", nrow(gravity_extended), "rows\n")
  cat("Years now covered:", paste(range(gravity_extended$t), collapse = " to "), "\n")
  
  return(gravity_extended)
}

#' Merge gravity variables with trade data for one year
merge_year <- function(year, gravity_data, trade_data_path = TRADE_DATA_PATH) {
  
  cat("\n=== Processing Year", year, "===\n")
  
  # Find trade data file
  trade_file <- file.path(
    trade_data_path, 
    paste0("BACI_HS02_Y", year, "_V202501_gravityready.csv")
  )
  
  if (!file.exists(trade_file)) {
    cat("WARNING: File not found:", trade_file, "\n")
    cat("Skipping year", year, "\n")
    return(NULL)
  }
  
  # Load trade data
  cat("Reading:", basename(trade_file), "\n")
  trade_data <- read_csv(trade_file, show_col_types = FALSE)
  cat("Trade data:", nrow(trade_data), "rows\n")
  
  # Get gravity data for this year
  gravity_year <- gravity_data %>%
    filter(t == year)
  
  if (nrow(gravity_year) == 0) {
    cat("WARNING: No gravity data for year", year, "\n")
    return(NULL)
  }
  
  cat("Gravity data for", year, ":", nrow(gravity_year), "rows\n")
  
  # Merge
  cat("Merging...\n")
  trade_with_gravity <- trade_data %>%
    left_join(
      gravity_year,
      by = c("i", "j", "t")
    )
  
  # Check merge success
  merge_success <- sum(!is.na(trade_with_gravity$distw_harmonic) | 
                         !is.na(trade_with_gravity$dist))
  merge_rate <- round(100 * merge_success / nrow(trade_with_gravity), 1)
  
  cat("Merge success:", merge_success, "of", nrow(trade_with_gravity),
      "rows (", merge_rate, "%)\n")
  
  # Report unmatched pairs
  unmatched <- trade_with_gravity %>%
    filter(is.na(distw_harmonic) & is.na(dist)) %>%
    select(i, j, t) %>%
    distinct()
  
  if (nrow(unmatched) > 0) {
    cat("WARNING:", nrow(unmatched), "country pairs without gravity data\n")
    if (nrow(unmatched) <= 10) {
      cat("Unmatched pairs:\n")
      print(unmatched)
    }
  }
  
  # Create backup
  backup_file <- file.path(
    trade_data_path,
    paste0("BACI_HS02_Y", year, "_V202501_gravityready_BACKUP.csv")
  )
  
  if (!file.exists(backup_file)) {
    cat("Creating backup...\n")
    file.copy(trade_file, backup_file)
  }
  
  # Save merged data
  output_file <- file.path(
    trade_data_path,
    paste0("BACI_HS02_Y", year, "_V202501_gravityready.csv")
  )
  
  cat("Saving to:", basename(output_file), "\n")
  write_csv(trade_with_gravity, output_file)
  
  # Summary statistics
  cat("\n--- Summary ---\n")
  cat("Final dimensions:", nrow(trade_with_gravity), "rows,", 
      ncol(trade_with_gravity), "columns\n")
  cat("Columns added:", ncol(trade_with_gravity) - ncol(trade_data), "\n")
  
  return(trade_with_gravity)
}

#' Validate merged data
validate_merged_data <- function(year, trade_data_path = TRADE_DATA_PATH) {
  
  trade_file <- file.path(
    trade_data_path,
    paste0("BACI_HS02_Y", year, "_V202501_gravityready.csv")
  )
  
  if (!file.exists(trade_file)) {
    return(NULL)
  }
  
  data <- read_csv(trade_file, show_col_types = FALSE, n_max = 1)
  
  # Check for key variables
  key_vars <- c("i", "j", "k", "t", "v", "distw_harmonic", "contig", "comlang_off")
  has_vars <- key_vars[key_vars %in% names(data)]
  missing_vars <- key_vars[!key_vars %in% names(data)]
  
  return(list(
    year = year,
    has_vars = has_vars,
    missing_vars = missing_vars
  ))
}

# Main Execution ===============================================================

main <- function() {
  
  cat("========================================\n")
  cat("MERGE GRAVITY VARIABLES WITH TRADE DATA\n")
  cat("========================================\n\n")
  
  # Step 1: Load gravity data
  gravity_data <- load_gravity_data()
  
  # Step 2: Load country mapping
  country_mapping <- load_country_mapping()
  
  # Step 3: Prepare gravity data for merge
  gravity_prep <- prepare_gravity_for_merge(gravity_data, country_mapping)
  
  # Step 4: Extend to 2022
  gravity_extended <- extend_gravity_to_2022(gravity_prep)
  
  # Step 5: Merge with each year's trade data
  cat("\n========================================\n")
  cat("MERGING WITH YEARLY TRADE FILES\n")
  cat("========================================\n")
  
  results <- list()
  for (year in YEARS) {
    results[[as.character(year)]] <- merge_year(year, gravity_extended)
  }
  
  # Step 6: Validate
  cat("\n========================================\n")
  cat("VALIDATION\n")
  cat("========================================\n\n")
  
  validations <- map(YEARS, validate_merged_data)
  successful <- sum(map_lgl(validations, ~!is.null(.x)))
  
  cat("Successfully processed:", successful, "of", length(YEARS), "years\n")
  
  # Check consistency
  cat("\nVariable consistency check:\n")
  all_vars <- map(validations[!map_lgl(validations, is.null)], "has_vars")
  common_vars <- Reduce(intersect, all_vars)
  cat("Variables present in all files:", length(common_vars), "\n")
  cat("Common variables:", paste(common_vars, collapse = ", "), "\n")
  
  cat("\n========================================\n")
  cat("MERGE COMPLETE\n")
  cat("========================================\n")
  cat("\nYour trade data files now include gravity variables!\n")
  cat("Next step: Estimate gravity model using PPML\n\n")
  
  return(invisible(results))
}

# Run
if (interactive()) {
  cat("Script loaded. Run main() to execute.\n")
} else {
  main()
}