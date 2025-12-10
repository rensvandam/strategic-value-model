# =============================================================================
# BACI DATA DOWNLOAD AND AGGREGATION
# Purpose: Download BACI HS6 trade data and aggregate to ISIC sectors and countries
# =============================================================================

library(tidyverse)
library(readxl)
library(data.table)  # For faster processing of large data
library(dplyr)
library(readr)
library(stringr)
library(readxl)
# =============================================================================
# 1. SETUP DIRECTORIES
# =============================================================================

dir.create("data", showWarnings = FALSE)
dir.create("data/baci", showWarnings = FALSE)
dir.create("data/concordances", showWarnings = FALSE)
dir.create("data/processed", showWarnings = FALSE)

# =============================================================================
# 2. DOWNLOAD BACI DATA (HS02, 2002-2023)
# =============================================================================
options(timeout = 1800)
download_baci <- function() {
  
  baci_url <- "https://www.cepii.fr/DATA_DOWNLOAD/baci/data/BACI_HS02_V202501.zip"
  zip_file <- "data/baci/BACI_HS02_V202501.zip"
  
  if(!file.exists(zip_file)) {
    cat("Downloading BACI data (this may take a while - ~1-2 GB)...\n")
    download.file(baci_url, zip_file, mode = "wb")
    cat("Download complete!\n")
  } else {
    cat("BACI zip file already exists, skipping download.\n")
  }
  
  # Unzip
  cat("Extracting files...\n")
  unzip(zip_file, exdir = "data/baci")
  
  cat("BACI data ready!\n")
}

# Run download
download_baci()

# =============================================================================
# 3. LOAD BACI DATA (EFFICIENT METHOD FOR LARGE FILES)
# =============================================================================

load_baci <- function(year_range = 2002:2023) {
  
  # BACI files are typically named like: BACI_HS02_Y2000.csv
  # Adjust the pattern based on actual file names
  
  baci_files <- list.files("data/baci", 
                           pattern = "\\.csv$", 
                           full.names = TRUE)
  
  cat("Found", length(baci_files), "BACI files\n")
  
  # Load and combine all years
  baci_data <- map_df(baci_files, function(file) {
    cat("Loading:", basename(file), "\n")
    
    # Use fread for speed (data.table)
    df <- fread(file, 
                select = c("t", "i", "j", "k", "v", "q"))  # Adjust column names as needed
    
    # BACI columns typically:
    # t = year, i = exporter, j = importer, k = HS6 product code
    # v = value (thousands USD), q = quantity
    
    return(df)
  })
  
  # Filter to desired years
  baci_data <- baci_data %>%
    filter(t >= min(year_range) & t <= max(year_range))
  
  cat("\nLoaded BACI data:\n")
  cat("Rows:", nrow(baci_data), "\n")
  cat("Years:", paste(range(baci_data$t), collapse = " to "), "\n")
  
  return(baci_data)
}

# =============================================================================
# 4. LOAD HS6 TO ISIC CONCORDANCE
# =============================================================================

load_concordance <- function(concordance_file) {
  
  # Load your Excel concordance file
  concordance <- read_excel(concordance_file, sheet=2)
  # Assuming columns are: product, Desci4
  # Standardize column names
  concordance <- concordance %>%
    select(hs6 = 'product', isic = 'Desci4') %>%
    mutate(
      product = as.character(hs6),
      Desci4 = as.character(isic)
    )
  
  cat("Concordance loaded:\n")
  cat("HS6 codes:", n_distinct(concordance$hs6), "\n")
  cat("ISIC codes:", n_distinct(concordance$isic), "\n")
  
  return(concordance)
}

# Load concordance
concordance <- load_concordance("data/concordances/BTDIxEConvKey.xlsx")

# Preview
head(concordance)

# =============================================================================
# 5. DEFINE TOP COUNTRIES AND REGIONS TO AGGREGATE ON AND AGGREGATE
# =============================================================================

aggregate_baci_countries <- function(
    input_dir = "data/baci",
    output_dir = "data/baci-agg",
    country_codes_file = "data/baci/country_codes_V202501.csv",
    top_n = 100,
    reference_year = 2023
) {

  
  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  
  # Read country codes
  country_codes <- read_csv(country_codes_file)
  
  # Read only the reference year to calculate rankings
  cat(sprintf("Reading %d data to identify top countries...\n", reference_year))
  
  reference_file <- file.path(input_dir, sprintf("BACI_HS02_Y%d_V202501.csv", reference_year))
  
  if (!file.exists(reference_file)) {
    stop(sprintf("Reference file not found: %s", reference_file))
  }
  
  reference_data <- read_csv(reference_file, show_col_types = FALSE)
  
  # Calculate country ranking based on reference year only
  country_ranking <- reference_data %>%
    group_by(i) %>%
    summarise(total_trade = sum(v, na.rm = TRUE)) %>%
    arrange(desc(total_trade)) %>%
    mutate(rank = row_number())
  
  # Get top N countries
  top_countries <- country_ranking %>%
    slice(1:top_n) %>%
    pull(i)
  
  cat(sprintf("\nTop %d countries identified based on %d trade data.\n", top_n, reference_year))
  cat("Top 10:\n")
  print(country_ranking %>% slice(1:10))
  
  # Read regional mapping from CSV
  regions_df <- read_csv("data/concordances/regions.csv")
  
  # Define regional groups for remaining countries
  region_mapping <- country_codes %>%
    filter(!(country_code %in% top_countries)) %>%
    left_join(regions_df, by = "country_code") %>%
    mutate(
      # Map region values (1-5) to region codes (9001-9005)
      region_code = case_when(
        region == 1 ~ 9001,  # Rest of Europe
        region == 2 ~ 9002,  # Rest of Asia
        region == 3 ~ 9003,  # Rest of Africa
        region == 4 ~ 9004,  # Rest of Americas
        region == 5 ~ 9005,  # Else
        TRUE ~ 9005          # Default to Else if missing
      )
    ) %>%
    select(country_code, region_code)
  
  # Process each year
  cat("\nAggregating trade data by year...\n")
  
  all_files <- list.files(input_dir, pattern = "BACI_HS02_Y.*_V202501\\.csv", full.names = TRUE)
  
  for (file in all_files) {
    year <- str_extract(basename(file), "(?<=Y)\\d{4}")
    cat(sprintf("Processing year %s...\n", year))
    
    data <- read_csv(file, show_col_types = FALSE)
    
    # Aggregate data
    aggregated_data <- data %>%
      left_join(region_mapping %>% rename(i = country_code, i_region = region_code), 
                by = "i") %>%
      left_join(region_mapping %>% rename(j = country_code, j_region = region_code), 
                by = "j") %>%
      mutate(
        i_agg = ifelse(i %in% top_countries, i, coalesce(i_region, 9005)),
        j_agg = ifelse(j %in% top_countries, j, coalesce(j_region, 9005))
      ) %>%
      group_by(i_agg, j_agg, k, t) %>%
      summarise(
        v = sum(v, na.rm = TRUE),
        q = sum(q, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      rename(i = i_agg, j = j_agg)
    
    # Save aggregated file
    output_file <- file.path(output_dir, basename(file))
    write_csv(aggregated_data, output_file)
    
    cat(sprintf("  Saved to %s\n", output_file))
    cat(sprintf("  Original rows: %d, Aggregated rows: %d\n", 
                nrow(data), nrow(aggregated_data)))
  }
  
  # Save mapping for reference
  region_names <- data.frame(
    region_code = c(9001, 9002, 9003, 9004, 9005),
    region_name = c("Rest of Asia", "Rest of Africa", "Rest of Americas", 
                    "Rest of Europe", "Small Islands & Others")
  )
  
  mapping_summary <- bind_rows(
    country_ranking %>% 
      filter(i %in% top_countries) %>%
      left_join(country_codes %>% select(country_code, country_name), 
                by = c("i" = "country_code")) %>%
      select(code = i, name = country_name, total_trade, rank),
    region_names %>%
      select(code = region_code, name = region_name) %>%
      mutate(total_trade = NA, rank = NA)
  )
  
  write_csv(mapping_summary, file.path(output_dir, "country_mapping.csv"))
  write_csv(country_ranking, file.path(output_dir, "country_ranking.csv"))
  
  cat("\nAggregation complete!\n")
  cat(sprintf("Files saved to: %s\n", output_dir))
  cat(sprintf("Country rankings based on %d trade data.\n", reference_year))
  
  return(list(
    top_countries = top_countries,
    country_ranking = country_ranking,
    region_mapping = region_mapping
  ))
}

# Run the function
result <- aggregate_baci_countries()

# =============================================================================
# 7. AGGREGATE BACI HS6 CODES TO ISIC INDUSTRY CATEGORIES
# =============================================================================

aggregate_hs6_to_isic <- function(
    input_dir = "data/baci-agg",
    concordance_file = "data/concordances/BTDIxEConvKey.xlsx",
    industry_codes_file = "data/concordances/industry_codes.csv",
    backup = TRUE
) {
  
  # Load HS6 to ISIC concordance
  cat("Loading HS6 to ISIC concordance...\n")
  concordance <- read_excel(concordance_file, sheet = 2) %>%
    select(hs6 = 'product', isic = 'Desci4') %>%
    mutate(
      hs6 = as.character(hs6),
      isic = as.character(isic)
    )
  
  cat(sprintf("Concordance loaded: %d HS6 codes -> %d ISIC codes\n", 
              n_distinct(concordance$hs6), 
              n_distinct(concordance$isic)))
  
  # Load industry code replacements
  cat("Loading industry code replacements...\n")
  industry_replacements <- read_csv(industry_codes_file, show_col_types = FALSE) %>%
    rename(BTDI = 1, conversion = 2) %>%
    mutate(
      BTDI = as.character(BTDI),
      conversion = as.character(conversion)
    )
  
  cat(sprintf("Industry replacements loaded: %d codes to be replaced\n", 
              nrow(industry_replacements)))
  
  # Preview replacements
  cat("\nReplacement mapping preview:\n")
  print(head(industry_replacements, 10))
  
  # Create backup directory if requested
  if (backup) {
    backup_dir <- file.path(input_dir, "backup_hs6")
    if (!dir.exists(backup_dir)) {
      dir.create(backup_dir, recursive = TRUE)
      cat(sprintf("\nCreated backup directory: %s\n", backup_dir))
    }
  }
  
  # Get all BACI files
  all_files <- list.files(input_dir, 
                          pattern = "BACI_HS02_Y.*_V202501\\.csv", 
                          full.names = TRUE)
  
  if (length(all_files) == 0) {
    stop("No BACI files found in ", input_dir)
  }
  
  cat(sprintf("\nProcessing %d files...\n", length(all_files)))
  
  # Track statistics
  stats <- data.frame(
    year = character(),
    original_rows = numeric(),
    after_isic_agg = numeric(),
    after_replacement = numeric(),
    after_filtering = numeric(),
    unmatched_hs6 = numeric(),
    codes_replaced = numeric(),
    n_final_isic_codes = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (file in all_files) {
    year <- str_extract(basename(file), "(?<=Y)\\d{4}")
    cat(sprintf("\n=== Processing year %s ===\n", year))
    
    # Backup original file
    if (backup) {
      backup_file <- file.path(dirname(file), "backup_hs6", basename(file))
      file.copy(file, backup_file, overwrite = TRUE)
      cat(sprintf("  Backed up to: %s\n", backup_file))
    }
    
    # Read data
    data <- read_csv(file, show_col_types = FALSE)
    original_rows <- nrow(data)
    
    # Convert k to character for matching
    data <- data %>%
      mutate(k = as.character(k))
    
    # Join with concordance
    data_with_isic <- data %>%
      left_join(concordance, by = c("k" = "hs6"))
    
    # Check for unmatched codes
    unmatched <- data_with_isic %>%
      filter(is.na(isic))
    
    if (nrow(unmatched) > 0) {
      cat(sprintf("  WARNING: %d rows with unmatched HS6 codes (%.2f%% of data)\n", 
                  nrow(unmatched), 
                  100 * nrow(unmatched) / nrow(data)))
      cat(sprintf("  Unique unmatched HS6 codes: %d\n", 
                  n_distinct(unmatched$k)))
    }
    
    # Step 1: Aggregate to ISIC level
    cat("\n  Step 1: Aggregating to ISIC level...\n")
    aggregated_data <- data_with_isic %>%
      filter(!is.na(isic)) %>%  # Remove unmatched codes
      group_by(i, j, k = isic, t) %>%
      summarise(
        v = sum(v, na.rm = TRUE),
        q = sum(q, na.rm = TRUE),
        .groups = "drop"
      )
    
    after_isic_rows <- nrow(aggregated_data)
    cat(sprintf("  After ISIC aggregation: %d rows\n", after_isic_rows))
    
    # Step 2: Replace industry codes
    cat("\n  Step 2: Replacing industry codes...\n")
    codes_before <- n_distinct(aggregated_data$k)
    
    aggregated_data <- aggregated_data %>%
      left_join(industry_replacements, by = c("k" = "BTDI")) %>%
      mutate(k = if_else(!is.na(conversion), conversion, k)) %>%
      select(-conversion)
    
    codes_replaced <- codes_before - n_distinct(aggregated_data$k)
    cat(sprintf("  Industry codes before replacement: %d\n", codes_before))
    cat(sprintf("  Industry codes after replacement: %d\n", n_distinct(aggregated_data$k)))
    cat(sprintf("  Codes consolidated: %d\n", codes_replaced))
    
    # Re-aggregate after replacement (codes may have been merged)
    cat("\n  Step 3: Re-aggregating after code replacement...\n")
    aggregated_data <- aggregated_data %>%
      group_by(i, j, k, t) %>%
      summarise(
        v = sum(v, na.rm = TRUE),
        q = sum(q, na.rm = TRUE),
        .groups = "drop"
      )
    
    after_replacement_rows <- nrow(aggregated_data)
    cat(sprintf("  After re-aggregation: %d rows\n", after_replacement_rows))
    
    # Step 4: Filter to keep only D10 to D32
    cat("\n  Step 4: Filtering to keep only D10-D32...\n")
    
    # Extract numeric part and filter
    aggregated_data <- aggregated_data %>%
      mutate(code_num = as.numeric(str_extract(k, "(?<=D)\\d+"))) %>%
      filter(!is.na(code_num), code_num >= 10, code_num <= 32) %>%
      select(-code_num)
    
    after_filtering_rows <- nrow(aggregated_data)
    final_codes <- n_distinct(aggregated_data$k)
    
    cat(sprintf("  After filtering D10-D32: %d rows\n", after_filtering_rows))
    cat(sprintf("  Final number of ISIC codes: %d\n", final_codes))
    cat(sprintf("  Final ISIC codes: %s\n", paste(sort(unique(aggregated_data$k)), collapse = ", ")))
    
    # Save aggregated file (overwrite original)
    write_csv(aggregated_data, file)
    
    # Summary for this year
    cat(sprintf("\n  === Year %s Summary ===\n", year))
    cat(sprintf("  Original rows: %d\n", original_rows))
    cat(sprintf("  Final rows: %d (%.1f%% reduction)\n", 
                after_filtering_rows, 
                100 * (1 - after_filtering_rows/original_rows)))
    cat(sprintf("  Final ISIC codes: %d\n", final_codes))
    
    # Store statistics
    stats <- bind_rows(stats, data.frame(
      year = year,
      original_rows = original_rows,
      after_isic_agg = after_isic_rows,
      after_replacement = after_replacement_rows,
      after_filtering = after_filtering_rows,
      unmatched_hs6 = nrow(unmatched),
      codes_replaced = codes_replaced,
      n_final_isic_codes = final_codes,
      stringsAsFactors = FALSE
    ))
  }
  
  # Save summary statistics
  summary_file <- file.path(input_dir, "aggregation_summary_isic.csv")
  write_csv(stats, summary_file)
  cat(sprintf("\n=== Summary statistics saved to: %s ===\n", summary_file))
  
  cat("\n=== AGGREGATION COMPLETE ===\n")
  cat(sprintf("Total files processed: %d\n", nrow(stats)))
  cat(sprintf("Average row reduction: %.1f%%\n", 
              100 * mean(1 - stats$after_filtering/stats$original_rows)))
  cat(sprintf("Average final ISIC codes per year: %.1f\n", 
              mean(stats$n_final_isic_codes)))
  
  # Overall summary
  cat("\nOverall statistics:\n")
  print(stats)
  
  return(stats)
}

# Run the function
stats <- aggregate_hs6_to_isic()

# View detailed summary
print(stats)

# Check which ISIC codes are in the final dataset
final_codes <- read_csv("data/baci-agg/BACI_HS02_Y2015_V202501.csv") %>%
  distinct(k) %>%
  arrange(k)

cat("\nFinal ISIC codes in dataset:\n")
print(final_codes)










##################################################
###### CODE TO RESTORE BACKUP (backup-hs6) #######

# First, restore the original HS6 files from backup
#restore_from_backup <- function(input_dir = "data/baci-agg") {
#  
#  backup_dir <- file.path(input_dir, "backup_hs6")
#  
#  if (!dir.exists(backup_dir)) {
#    stop("No backup directory found. You need the original HS6 files.")
#  }
#  
#  backup_files <- list.files(backup_dir, 
#                             pattern = "BACI_HS02_Y.*_V202501\\.csv",
#                             full.names = TRUE)
#  
#  if (length(backup_files) == 0) {
#    stop("No backup files found in ", backup_dir)
#  }
#  
#  cat(sprintf("Restoring %d files from backup...\n", length(backup_files)))
#  
#  for (file in backup_files) {
#    dest_file <- file.path(input_dir, basename(file))
#    file.copy(file, dest_file, overwrite = TRUE)
#    cat(sprintf("  Restored: %s\n", basename(file)))
#  }
#  
#  cat("\nRestore complete!\n")
#}

# Restore original files
#restore_from_backup()

