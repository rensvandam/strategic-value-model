# Option 2: Download CEPII's full Gravity database
library(haven)

# This has data from 1948-2020 (every year!)
# Download and extract
# Create a data directory in your project
dir.create("data", showWarnings = FALSE)

# Download
download.file("https://www.cepii.fr/DATA_DOWNLOAD/gravity/data/Gravity_rds_V202211.zip",
              destfile = "data/Gravity_rds_V202211.zip",
              mode = "wb")

# Unzip
unzip("data/Gravity_rds_V202211.zip", exdir = "data")

# Read
gravity <- readRDS("data/Gravity_V202211.rds")

# Check years
unique(gravity$year)
names(gravity)

# Filter to CPB paper period (2000-2021)
gravity_2000_2015 <- gravity %>%
  filter(year >= 2000 & year <= 2022)