# Install packages (5 minutes)
install.packages(c("gravity", "tidyverse", "haven"))

# Download CEPII gravity data (10 minutes)
# Go to: http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8
# Download "Gravity_csv_V202211.zip"

# Quick start (30 minutes)
library(gravity)
library(tidyverse)

# Load data
grav <- read_csv("data/Gravity_csv_V202211/Gravity_V202211.csv")

# Filter Netherlands
# Note: BACI trade data only available 1996-2019, using 2015 instead of 2017
nl_data <- grav %>%
  filter(iso3_o == "NLD" | iso3_d == "NLD") %>%
  filter(year == 2015)

# Check data quality for all trade flow variables
print(paste("Total rows:", nrow(nl_data)))
print("\nTrade flow variables:")
print(paste("Rows with non-NA tradeflow_baci:", sum(!is.na(nl_data$tradeflow_baci))))
print(paste("Rows with non-NA manuf_tradeflow_baci:", sum(!is.na(nl_data$manuf_tradeflow_baci))))
print(paste("Rows with non-NA tradeflow_comtrade_o:", sum(!is.na(nl_data$tradeflow_comtrade_o))))
print(paste("Rows with non-NA tradeflow_comtrade_d:", sum(!is.na(nl_data$tradeflow_comtrade_d))))
print(paste("Rows with non-NA tradeflow_imf_o:", sum(!is.na(nl_data$tradeflow_imf_o))))
print(paste("Rows with non-NA tradeflow_imf_d:", sum(!is.na(nl_data$tradeflow_imf_d))))

# Check what years have BACI data
print("\nChecking BACI data availability across all years:")
baci_check <- grav %>%
  filter(iso3_o == "NLD" | iso3_d == "NLD") %>%
  group_by(year) %>%
  summarise(
    total_rows = n(),
    baci_available = sum(!is.na(tradeflow_baci)),
    comtrade_o_available = sum(!is.na(tradeflow_comtrade_o)),
    comtrade_d_available = sum(!is.na(tradeflow_comtrade_d))
  )
print(baci_check)

# Try using a year with BACI data, or use different trade flow variable
# For now, let's try 2015 (BACI data often lags 2 years)
nl_data_2015 <- grav %>%
  filter(iso3_o == "NLD" | iso3_d == "NLD") %>%
  filter(year == 2015)

print(paste("\n2015 data - Rows with non-NA tradeflow_baci:", sum(!is.na(nl_data_2015$tradeflow_baci))))

# This gives you baseline model in 45 minutes total
# Now you can start building scenarios