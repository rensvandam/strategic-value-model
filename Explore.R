# Exploring the data

library(gravity)
library(tidyverse)
ls("package:gravity")
data("gravity_zeros")
df <- gravity_no_zeros

# ===== BASIC STRUCTURE =====

# Dimensions
dim(df)           # rows and columns

# First/last rows
head(df, 10)      # first 10 rows
tail(df)          # last 6 rows

# Structure overview
str(df)           # data types and preview
glimpse(df)       # tidyverse version (better formatting)

# Column names
names(df)
colnames(df)

# Summary statistics
summary(df)       # min, max, median, mean for numeric columns

# ===== DESCRIPTIVE STATISTICS =====

# More detailed statistics
library(psych)
describe(df)

# Tidyverse approach
df %>%
  summarise(across(where(is.numeric),
                   list(mean = ~mean(., na.rm = TRUE),
                        sd = ~sd(., na.rm = TRUE),
                        min = ~min(., na.rm = TRUE),
                        max = ~max(., na.rm = TRUE))))

# Statistics by group
df %>%
  group_by(year) %>%
  summarise(mean_trade = mean(trade, na.rm = TRUE),
            median_trade = median(trade, na.rm = TRUE),
            n = n())

# Quantiles
quantile(df$trade, probs = c(0.25, 0.5, 0.75, 0.9, 0.95), na.rm = TRUE)

# ===== YEAR EXPLORATION =====

# Unique years
unique(df$year)
sort(unique(df$year))

# Year range
range(df$year)
min(df$year)
max(df$year)

# Count observations per year
table(df$year)
df %>% count(year)

# Year coverage with plot
df %>% 
  count(year) %>%
  ggplot(aes(x = year, y = n)) +
  geom_line() +
  labs(title = "Observations per Year")

# Check if years are continuous
all(diff(sort(unique(df$year))) == 1)  # TRUE if consecutive
