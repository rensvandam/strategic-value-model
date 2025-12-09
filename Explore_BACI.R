# Explore BACI files

data <- read.csv("./data/baci/BACI_HS02_Y2023_V202501.csv")

dim(data)
names(data)
head(data, 20)

define_country_groups <- function(baci_data) {
  
  # Calculate total trade by country (as exporter + importer)
  country_trade <- baci_data %>%
    group_by(i) %>%
    summarise(export_value = sum(v, na.rm = TRUE)) %>%
    arrange(desc(export_value)) %>%
    mutate(rank = row_number())
  
  cat("Countries ordered by trade volume (economy size):\n")
  cat(sprintf("Total countries: %d\n\n", nrow(country_trade)))
  
  # Show top 10 as preview
  cat("Top 10 countries:\n")
  print(head(country_trade, 10))
  
  return(country_trade)
}

all_countries <- define_country_groups(data)
