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
    country_codes_file = "country_codes_V202501.csv",
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
  
  # Define regional groups for remaining countries
  region_mapping <- country_codes %>%
    filter(!(country_code %in% top_countries)) %>%
    mutate(
      region_code = case_when(
        # Asia (excluding top countries)
        country_code %in% c(4, 50, 64, 96, 144, 360, 364, 392, 398, 408, 410, 417, 418, 
                            458, 462, 496, 104, 524, 586, 608, 702, 764, 762, 626, 704, 
                            860, 887, 446, 344, 583, 584, 585) ~ 9001,
        # Africa
        country_code %in% c(12, 24, 72, 108, 120, 140, 148, 174, 175, 178, 180, 204, 
                            226, 231, 232, 262, 266, 270, 288, 324, 384, 404, 426, 430, 
                            434, 450, 454, 466, 478, 480, 504, 508, 516, 562, 566, 646, 
                            678, 686, 690, 694, 706, 710, 716, 728, 729, 736, 768, 800, 
                            818, 834, 854, 894) ~ 9002,
        # Americas (excluding top countries)
        country_code %in% c(28, 44, 52, 84, 92, 136, 188, 212, 214, 218, 222, 308, 
                            312, 320, 328, 332, 340, 388, 558, 591, 600, 604, 630, 
                            659, 660, 662, 670, 740, 780, 796, 850, 858, 862) ~ 9003,
        # Europe (excluding top countries)
        country_code %in% c(8, 20, 31, 40, 51, 70, 100, 112, 191, 196, 203, 208, 
                            233, 234, 246, 268, 292, 300, 304, 336, 348, 352, 372, 
                            428, 438, 440, 442, 470, 492, 498, 499, 500, 528, 578, 
                            616, 620, 642, 674, 688, 703, 705, 724, 744, 752, 757, 
                            795, 804, 807, 826, 891) ~ 9004,
        # Small Island States & Others
        TRUE ~ 9005
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
# 7. AGGREGATE BACI DATA
# =============================================================================

aggregate_baci <- function(baci_data, concordance, top_50_countries) {
  
  cat("Starting aggregation...\n")
  
  # Step 1: Add ISIC codes
  cat("Step 1: Merging HS6 to ISIC concordance...\n")
  baci_with_isic <- baci_data %>%
    mutate(k = as.character(k)) %>%  # HS6 code
    left_join(concordance, by = c("k" = "hs6"))
  
  # Check for unmatched HS6 codes
  unmatched <- sum(is.na(baci_with_isic$isic))
  cat("Unmatched HS6 codes:", unmatched, 
      "(", round(unmatched/nrow(baci_with_isic)*100, 2), "%)\n")
  
  # Remove unmatched
  baci_with_isic <- baci_with_isic %>%
    filter(!is.na(isic))
  
  # Step 2: Assign country groups (top 50 + RoW regions)
  cat("Step 2: Assigning country groups...\n")
  baci_grouped <- baci_with_isic %>%
    mutate(
      exporter_group = map_chr(i, ~assign_row_regions(.x, top_50_countries)),
      importer_group = map_chr(j, ~assign_row_regions(.x, top_50_countries))
    )
  
  # Step 3: Aggregate to ISIC sector and country group level
  cat("Step 3: Aggregating to ISIC sectors and country groups...\n")
  baci_aggregated <- baci_grouped %>%
    group_by(t, exporter_group, importer_group, isic) %>%
    summarise(
      trade_value = sum(v, na.rm = TRUE),
      trade_quantity = sum(q, na.rm = TRUE),
      n_hs6_products = n_distinct(k),
      .groups = 'drop'
    ) %>%
    rename(
      year = t,
      exporter = exporter_group,
      importer = importer_group,
      sector = isic
    )
  
  cat("\nAggregation complete!\n")
  cat("Final dataset:\n")
  cat("Rows:", nrow(baci_aggregated), "\n")
  cat("Years:", paste(range(baci_aggregated$year), collapse = " to "), "\n")
  cat("Exporters:", n_distinct(baci_aggregated$exporter), "\n")
  cat("Importers:", n_distinct(baci_aggregated$importer), "\n")
  cat("Sectors:", n_distinct(baci_aggregated$sector), "\n")
  
  return(baci_aggregated)
}

# =============================================================================
# 8. EXECUTE FULL PIPELINE
# =============================================================================

run_baci_aggregation <- function() {
  
  # Load BACI data
  cat("\n=== LOADING BACI DATA ===\n")
  baci <- load_baci(year_range = 2000:2015)
  
  # Define top 50 countries
  cat("\n=== DEFINING COUNTRY GROUPS ===\n")
  top_50 <- define_country_groups(baci)
  
  # Load concordance (make sure file exists!)
  cat("\n=== LOADING CONCORDANCE ===\n")
  concordance <- load_concordance("data/concordances/hs6_to_isic.xlsx")
  
  # Aggregate
  cat("\n=== AGGREGATING DATA ===\n")
  baci_final <- aggregate_baci(baci, concordance, top_50)
  
  # Save processed data
  cat("\n=== SAVING PROCESSED DATA ===\n")
  saveRDS(baci_final, "data/processed/baci_aggregated_isic.rds")
  write_csv(baci_final, "data/processed/baci_aggregated_isic.csv")
  
  cat("\nProcessing complete! Files saved in data/processed/\n")
  
  return(baci_final)
}

# =============================================================================
# 9. RUN THE PIPELINE
# =============================================================================

# Execute (this will take time for large files)
baci_aggregated <- run_baci_aggregation()

# =============================================================================
# 10. EXPLORE AGGREGATED DATA
# =============================================================================

# Quick exploration
glimpse(baci_aggregated)

# Summary statistics
baci_aggregated %>%
  group_by(year) %>%
  summarise(
    total_trade = sum(trade_value),
    n_flows = n(),
    n_exporters = n_distinct(exporter),
    n_importers = n_distinct(importer)
  )

# Top sectors by trade value
baci_aggregated %>%
  group_by(sector) %>%
  summarise(total_trade = sum(trade_value)) %>%
  arrange(desc(total_trade))

# Top country pairs
baci_aggregated %>%
  group_by(exporter, importer) %>%
  summarise(total_trade = sum(trade_value), .groups = 'drop') %>%
  arrange(desc(total_trade)) %>%
  head(20)

