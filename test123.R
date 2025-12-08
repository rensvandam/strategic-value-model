library(gravity)
ls("package:gravity")
#data(package = "gravity")

#load OLS from gravity package


data("gravity_zeros")
df <- gravity_zeros
str(df)
head(df)


# 1) Running OLS (loglinear)

# log-linear OLS (works only if trade > 0)

# rename flow to trade
names(df)[names(df) == "flow"] <- "trade"

df_sub <- subset(df, trade > 0)
model_ols <- gravity::OLS(
  formula = log(trade) ~ log(exporter_gdp) + log(importer_gdp) + log(dist) + contig + rta,
  data = df_sub
)

summary(model_ols)
