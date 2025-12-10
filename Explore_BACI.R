# Explore BACI files

baci_data <- read.csv("./data/baci-agg/BACI_HS02_Y2023_V202501.csv")

dim(data)
names(data)
head(data, 20)
# Unique values of a column
unique(data$k)
table(data$k)

################################################
##### DESCRIPTIVE STATISTICS OF BACI-AGG #######
################################################

library(tidyverse)
library(knitr)
library(kableExtra)

# =============================================================================
# LOAD DATA AND MAPPINGS
# =============================================================================

# Load manufacturing categories
manufacturing_categories <- read_csv("data/concordances/manufacturing_categories_D10_D32.csv")

# Load country codes from BACI
country_codes <- fread("data/baci/country_codes_V202501.csv") %>%
  select(country_code, country_iso3, country_name)

# Load your BACI data
# baci_data <- ... (your aggregated BACI data)

# =============================================================================
# TABLE 1: Industry Statistics with Largest Exporter/Importer
# =============================================================================

table1 <- baci_data %>%
  # Rename for clarity
  rename(exporter = i, importer = j, sector = k, year = t, value = v) %>%
  
  # Calculate global trade share by sector
  group_by(sector) %>%
  mutate(sector_total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(global_total = sum(value, na.rm = TRUE),
         global_share = sector_total / global_total) %>%
  
  # Find largest exporter per sector
  group_by(sector, exporter) %>%
  summarise(
    export_value = sum(value, na.rm = TRUE),
    global_share = first(global_share),
    .groups = 'drop'
  ) %>%
  group_by(sector) %>%
  slice_max(export_value, n = 1) %>%
  select(sector, global_share, largest_exporter = exporter) %>%
  
  # Find largest importer per sector
  left_join(
    baci_data %>%
      rename(exporter = i, importer = j, sector = k, value = v) %>%
      group_by(sector, importer) %>%
      summarise(import_value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
      group_by(sector) %>%
      slice_max(import_value, n = 1) %>%
      select(sector, largest_importer = importer),
    by = "sector"
  ) %>%
  
  # Join country names
  left_join(country_codes %>% select(country_code, exporter_iso3 = country_iso3),
            by = c("largest_exporter" = "country_code")) %>%
  left_join(country_codes %>% select(country_code, importer_iso3 = country_iso3),
            by = c("largest_importer" = "country_code")) %>%
  
  # ADD MANUFACTURING CATEGORY LABELS
  left_join(manufacturing_categories, by = c("sector" = "code")) %>%
  
  # Format
  mutate(global_share = scales::percent(global_share, accuracy = 0.01)) %>%
  select(
    Code = sector,
    Name = description,  # Added sector name
    `Global trade share` = global_share,
    `Largest exporter` = exporter_iso3,
    `Largest importer` = importer_iso3
  ) %>%
  arrange(Code)

# Print table
print(table1)

# Create formatted table with labels
table1 %>%
  kable(format = "html", align = c("l", "l", "r", "c", "c")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE)

# =============================================================================
# TABLE 2: Industry Export Shares by Country
# =============================================================================

# Define countries of interest
countries_of_interest <- c("AUS", "CAN", "CHN", "DEU", "FRA", "GBR", "IRL", "NLD", "USA")

table2 <- baci_data %>%
  rename(exporter = i, importer = j, sector = k, year = t, value = v) %>%
  
  # Join country codes
  left_join(country_codes %>% select(country_code, country_iso3),
            by = c("exporter" = "country_code")) %>%
  
  # Filter to countries of interest
  filter(country_iso3 %in% countries_of_interest) %>%
  
  # Calculate total exports by country
  group_by(country_iso3) %>%
  mutate(country_total_exports = sum(value, na.rm = TRUE)) %>%
  
  # Calculate exports by country-sector
  group_by(country_iso3, sector) %>%
  summarise(
    sector_exports = sum(value, na.rm = TRUE),
    country_total = first(country_total_exports),
    .groups = 'drop'
  ) %>%
  
  # Calculate share
  mutate(export_share = sector_exports / country_total) %>%
  
  # ADD MANUFACTURING CATEGORY LABELS BEFORE PIVOTING
  left_join(manufacturing_categories, by = c("sector" = "code")) %>%
  
  # Create combined label (code + name)
  mutate(sector_label = paste0(sector, " - ", description)) %>%
  
  # Pivot to wide format
  select(sector, sector_label, country_iso3, export_share) %>%
  pivot_wider(names_from = country_iso3, 
              values_from = export_share,
              values_fill = 0) %>%
  
  # Format as percentages
  mutate(across(-c(sector, sector_label), ~scales::percent(.x, accuracy = 0.1))) %>%
  
  # Arrange by sector code
  arrange(sector) %>%
  select(Industry = sector_label, everything(), -sector)

# Print table
print(table2)

# Create formatted table
table2 %>%
  kable(format = "html", align = c("l", rep("r", length(countries_of_interest)))) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"),
                full_width = FALSE,
                font_size = 12) %>%
  add_header_above(c(" " = 1, "Industry export shares by country" = length(countries_of_interest)))

# =============================================================================
# Save tables
# =============================================================================

write_csv(table1, "output/table1_industry_statistics.csv")
write_csv(table2, "output/table2_export_shares.csv")

