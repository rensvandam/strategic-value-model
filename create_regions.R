# Creating the rest regions

countries <- read_csv("./data/baci/country_codes_V202501.csv")


countries_ranked <- all_countries

# Filter countries to only keep those lower than place 100 in countries_ranked
countries_below_100 <- countries %>%
  left_join(countries_ranked, by = c("country_code" = "i")) %>%
  filter(rank > 100 | is.na(rank))

countries_below_100


region