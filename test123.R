library(gravity)
ls("package:gravity")
#data(package = "gravity")

#load OLS from gravity package


data("gravity_zeros")
df <- gravity_zeros
str(df)
head(df)

#######################################
### 1) Running OLS (log-linear) #######

# log-linear OLS (works only if trade > 0)

# rename flow to trade
names(df)[names(df) == "flow"] <- "trade"

df_sub$ln_trade <- log(df_sub$trade)
df_sub$ln_gdp_o <- log(df_sub$gdp_o)
df_sub$ln_gdp_d <- log(df_sub$gdp_d)
df_sub$ln_distw  <- log(df_sub$distw)

model_ols <- gravity::ols(
  dependent_variable   = "ln_trade",
  distance             = "ln_distw",
  income_origin        = "ln_gdp_o",
  income_destination   = "ln_gdp_d",
  code_origin = "iso_o",
  code_destination = "iso_d",
  additional_regressors = c("contig", "rta"),
  data = df_sub
)

summary(model_ols)

###############################################################
### 2) Running PPML (Poisson Pseudo-Maximum Likelihood) #######

model_ppml <- gravity::ppml(
  dependent_variable   = "trade",
  distance             = "ln_distw",
  #income_origin        = "ln_gdp_o",
  #income_destination   = "ln_gdp_d",
  #code_origin = "iso_o",
  #code_destination = "iso_d",
  additional_regressors = c("ln_gdp_o", "ln_gdp_d", "contig", "rta"),
  data = df_sub
)

summary(model_ppml)

################################################################
### 2) Including exporter/importer fixed effects in PPML #######

model_ppml <- gravity::ppml(
  dependent_variable   = "trade",
  distance             = "ln_distw",
  #income_origin        = "ln_gdp_o",
  #income_destination   = "ln_gdp_d",
  #code_origin = "iso_o",
  #code_destination = "iso_d",
  additional_regressors = c("origin_f", "dest_f", "contig", "rta"),
  data = df_sub
)

summary(model_ppml)