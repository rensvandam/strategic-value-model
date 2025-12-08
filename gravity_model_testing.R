# ==============================================================================
# GRAVITY MODEL TESTING AND LEARNING SCRIPT
# ==============================================================================
#
# Purpose: Testing and experimenting with gravity models for international trade
# Reference: https://imedkrisna.github.io/gravity/
#
# What are Gravity Models?
# Gravity models predict bilateral trade flows between countries based on:
#   - Economic size (GDP) - larger economies trade more
#   - Trade frictions (distance, borders, etc.) - more friction = less trade
#   - Similar to Newton's gravity: Trade ∝ (Size1 × Size2) / Distance
#
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. LOAD REQUIRED PACKAGES
# ------------------------------------------------------------------------------

# Install packages if needed (uncomment to run):
# install.packages(c("fixest", "tidyverse", "modelsummary", "gravity"))

# Install packages from CRAN (binary versions - no compilation needed):
#install.packages(c("stringmagic", "dreamerr", "Rcpp", "numDeriv", "writexl"))
# Note: This will get the latest versions available from CRAN
library(tidyverse)
library(writexl)
library(modelsummary)
# library(fixest)

#library(fixest)        # For PPML regression with fixed effects (fepois)
#library(tidyverse)     # For data manipulation (dplyr, ggplot2, etc.)
#library(modelsummary)  # For creating nice regression tables

# Optional: gravity package has built-in datasets and functions
# library(gravity)

# ------------------------------------------------------------------------------
# 2. LOAD REAL GRAVITY DATA FROM CEPII
# ------------------------------------------------------------------------------
#
# The CEPII Gravity dataset contains:
#   - Bilateral variables: distance, contiguity, common language, colonial ties
#   - Country-level variables: GDP, population for origin and destination
#   - Trade policy variables: FTA, WTO membership, EU membership
#   - Trade flow data: from various sources (BACI, COMTRADE, IMF)
#
# Dataset covers: 1948-2020, 285 countries/territories
# Full documentation: http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=8

# Load the full gravity dataset
# Note: This is a large file (4.7 million rows), may take a moment to load
gravity_full <- read_csv("data/Gravity_csv_V202211/Gravity_V202211.csv")

# Check the structure
glimpse(gravity_full)

# For learning purposes, let's work with a manageable subset
# Filter to recent years (2010-2020) and countries with trade flow data
trade_data <- gravity_full %>%
  filter(
    year >= 2000,                          # Recent decade
    year <= 2010,                          # Up to 2020
    !is.na(tradeflow_baci),               # Has BACI trade flow data
    tradeflow_baci > 0,                    # Positive trade flows
    iso3_o != iso3_d,                      # Exclude domestic "trade"
    !is.na(gdp_o), !is.na(gdp_d),         # Both countries have GDP data
    !is.na(dist)                           # Distance is available
  ) %>%
  # Select and rename key variables for clarity
  select(
    year,
    exporter = iso3_o,                     # Origin (exporter) ISO3 code
    importer = iso3_d,                     # Destination (importer) ISO3 code
    trade_flow = tradeflow_baci,           # Bilateral trade flow from BACI
    exporter_gdp = gdp_o,                  # Exporter GDP (current USD)
    importer_gdp = gdp_d,                  # Importer GDP (current USD)
    exporter_pop = pop_o,                  # Exporter population
    importer_pop = pop_d,                  # Importer population
    distance = dist,                       # Bilateral distance (pop-weighted, km)
    contiguity = contig,                   # Share a border (1=yes, 0=no)
    common_language = comlang_off,         # Common official language
    colonial_ties = comcol,                # Ever in colonial relationship
    fta = fta_wto                          # Free trade agreement (WTO notified)
  ) %>%
  # Create a country-pair identifier (for pair fixed effects)
  mutate(
    pair_id = paste(pmin(exporter, importer),
                    pmax(exporter, importer),
                    sep = "_")
  )

# Check how many observations we have
cat("Dataset size:", nrow(trade_data), "observations\n")
cat("Countries (exporters):", n_distinct(trade_data$exporter), "\n")
cat("Countries (importers):", n_distinct(trade_data$importer), "\n")
cat("Years:", min(trade_data$year), "-", max(trade_data$year), "\n")
cat("Country-pairs:", n_distinct(trade_data$pair_id), "\n")

# View the first few rows
head(trade_data)

# Check summary statistics
summary(trade_data$trade_flow)
summary(trade_data$distance)

# Look at FTA coverage
table(trade_data$fta, useNA = "ifany")

# ------------------------------------------------------------------------------
# 2B. EXPLORE THE REAL DATA
# ------------------------------------------------------------------------------

# Top 10 bilateral trade flows in 2020
# top_trade_2020 <- trade_data %>%
#   filter(year == 2020) %>%
#   arrange(desc(trade_flow)) %>%
#   select(exporter, importer, trade_flow, distance, fta) %>%
#   head(10)

# cat("\nTop 10 bilateral trade flows in 2020:\n")
# print(top_trade_2020)

# # Average trade by geographic relationship
# trade_by_geography <- trade_data %>%
#   group_by(contiguity, common_language) %>%
#   summarise(
#     avg_trade = mean(trade_flow),
#     median_trade = median(trade_flow),
#     n_pairs = n(),
#     .groups = "drop"
#   ) %>%
#   mutate(
#     relationship = case_when(
#       contiguity == 1 ~ "Neighbors",
#       common_language == 1 ~ "Common Language",
#       TRUE ~ "Other"
#     )
#   )

# cat("\nAverage trade by geographic/cultural relationship:\n")
# print(trade_by_geography)

# # FTA effect over time (simple comparison)
# fta_effect_time <- trade_data %>%
#   group_by(year, fta) %>%
#   summarise(avg_trade = mean(trade_flow), .groups = "drop") %>%
#   pivot_wider(names_from = fta, values_from = avg_trade, names_prefix = "fta_") %>%
#   mutate(fta_premium = fta_1 / fta_0)

# cat("\nFTA premium over time (simple ratio):\n")
# print(fta_effect_time)

# ------------------------------------------------------------------------------
# 3. BASIC GRAVITY MODEL - OLS WITH LOGS
# ------------------------------------------------------------------------------
#
# Traditional approach: log-linearize the gravity equation
# Problem: loses observations with zero trade flows
#

# Filter out any zero or negative trade flows for log transformation
# trade_data_positive <- trade_data %>%
#   filter(trade_flow > 0)

# # Basic OLS gravity model with logged variables
# model_ols <- feols(
#   log(trade_flow) ~ log(exporter_gdp) + log(importer_gdp) +
#                     log(distance) + contiguity + common_language +
#                     colonial_ties + fta,
#   data = trade_data_positive
# )

# # View results
# summary(model_ols)

# Interpretation:
# - Coefficients on log variables are elasticities
# - e.g., if log(distance) coef = -0.8, a 1% increase in distance
#   decreases trade by 0.8%
# - Binary variable coefficients: exp(coef) - 1 = percentage effect
#   e.g., if FTA coef = 0.5, exp(0.5) - 1 ≈ 65% increase in trade

# ------------------------------------------------------------------------------
# 4. PPML MODEL - PREFERRED MODERN APPROACH
# ------------------------------------------------------------------------------
#
# PPML = Poisson Pseudo-Maximum Likelihood
# Advantages:
#   - Handles zero trade flows naturally
#   - Addresses heteroskedasticity issues
#   - Consistent with multiplicative gravity equation
#
# Use fepois() for PPML regression

# model_ppml <- fepois(
#   trade_flow ~ log(exporter_gdp) + log(importer_gdp) +
#                log(distance) + contiguity + common_language +
#                colonial_ties + fta,
#   data = trade_data,  # Can use full dataset, including zeros
#   vcov = "hetero"     # Heteroskedasticity-robust standard errors
# )

# summary(model_ppml)

# # Compare OLS vs PPML
# modelsummary(
#   list("OLS (log)" = model_ols, "PPML" = model_ppml),
#   stars = TRUE
# )

# # ------------------------------------------------------------------------------
# # 5. ADDING FIXED EFFECTS - CONTROLLING FOR MULTILATERAL RESISTANCE
# # ------------------------------------------------------------------------------
# #
# # Multilateral Resistance Terms (MRT):
# #   - Countries trade based not just on bilateral costs, but on
# #     average trade costs with all partners
# #   - Solution: Add exporter and importer fixed effects
# #
# # Fixed effects notation in fixest:
# #   - | separates the main equation from fixed effects
# #   - fe1 + fe2 for multiple fixed effects
# #   - fe1^fe2 for interacted fixed effects

# # Model with exporter and importer fixed effects
# model_ppml_fe <- fepois(
#   trade_flow ~ log(distance) + contiguity + common_language +
#                colonial_ties + fta | exporter + importer,
#   data = trade_data,
#   vcov = "hetero"
# )

# summary(model_ppml_fe)

# # Note: GDP variables are absorbed by exporter/importer fixed effects
# # Fixed effects capture all time-invariant country characteristics

# # ------------------------------------------------------------------------------
# # 6. PANEL DATA WITH TIME-VARYING EFFECTS
# # ------------------------------------------------------------------------------
# #
# # For panel data (multiple years), add time dimension to fixed effects:
# #   - exporter^year: controls for exporter-year specific effects
# #   - importer^year: controls for importer-year specific effects
# #   - pair_id: controls for all time-invariant pair characteristics

# # Full structural gravity model with panel fixed effects
# model_structural <- fepois(
#   trade_flow ~ fta |                          # Only time-varying policy variable
#                exporter^year +                 # Exporter-time fixed effects
#                importer^year +                 # Importer-time fixed effects
#                pair_id,                        # Pair fixed effects
#   data = trade_data,
#   vcov = "hetero"
# )

# summary(model_structural)

# # Interpretation:
# # - FTA coefficient shows impact of trade agreements
# # - All other bilateral variables (distance, language, etc.) absorbed by pair FE
# # - This is the "gold standard" structural gravity specification

# # ------------------------------------------------------------------------------
# # 7. COMPARING MODELS
# # ------------------------------------------------------------------------------

# # Create a comparison table
# modelsummary(
#   list(
#     "OLS" = model_ols,
#     "PPML" = model_ppml,
#     "PPML + FE" = model_ppml_fe,
#     "Structural" = model_structural
#   ),
#   stars = TRUE,
#   gof_map = c("nobs", "r.squared", "adj.r.squared")
# )

# # ------------------------------------------------------------------------------
# # 8. VISUALIZING RESULTS
# # ------------------------------------------------------------------------------

# # Plot: Trade vs Distance (gravity relationship)
# ggplot(trade_data, aes(x = distance, y = trade_flow)) +
#   geom_point(alpha = 0.3) +
#   geom_smooth(method = "lm", color = "red") +
#   scale_x_log10() +
#   scale_y_log10() +
#   labs(
#     title = "Gravity Relationship: Trade vs Distance",
#     x = "Distance (km, log scale)",
#     y = "Trade Flow (log scale)"
#   ) +
#   theme_minimal()

# # Plot: FTA effect
# trade_summary <- trade_data %>%
#   group_by(fta) %>%
#   summarise(
#     mean_trade = mean(trade_flow),
#     se_trade = sd(trade_flow) / sqrt(n())
#   )

# ggplot(trade_summary, aes(x = factor(fta), y = mean_trade)) +
#   geom_col(fill = "steelblue") +
#   geom_errorbar(
#     aes(ymin = mean_trade - se_trade, ymax = mean_trade + se_trade),
#     width = 0.2
#   ) +
#   labs(
#     title = "Average Trade Flow by FTA Status",
#     x = "FTA (0 = No, 1 = Yes)",
#     y = "Average Trade Flow"
#   ) +
#   theme_minimal()

# # ------------------------------------------------------------------------------
# # 9. EXTRACTING AND EXAMINING FIXED EFFECTS
# # ------------------------------------------------------------------------------

# # Extract fixed effects from the model
# fe_effects <- fixef(model_ppml_fe)

# # View exporter fixed effects (captures exporter "outward multilateral resistance")
# head(fe_effects$exporter)

# # View importer fixed effects (captures importer "inward multilateral resistance")
# head(fe_effects$importer)

# # These represent country-specific trade propensities after controlling
# # for bilateral variables

# # ------------------------------------------------------------------------------
# # 10. NEXT STEPS AND ADVANCED ANALYSIS IDEAS
# # ------------------------------------------------------------------------------

# # Now that you have the real CEPII Gravity data, here are ways to extend:
# #
# # 1. FOCUS ON SPECIFIC QUESTIONS:
# #    - How did COVID-19 affect trade? (compare 2019 vs 2020)
# #    - Which FTAs had the largest impact?
# #    - How has the distance effect changed over time?
# #
# # 2. SECTOR-SPECIFIC ANALYSIS:
# #    - Use manuf_tradeflow_baci for manufacturing trade only
# #    - This can reveal different patterns (manufacturing more sensitive to FTAs)
# #
# # 3. REGIONAL FOCUS:
# #    regional_data <- trade_data %>%
# #      filter(exporter %in% c("USA", "CAN", "MEX"))  # NAFTA/USMCA
# #
# # 4. TIME-VARYING EFFECTS:
# #    # Interact FTA with year to see if effect grows over time
# #    model_fta_dynamic <- fepois(
# #      trade_flow ~ fta*factor(year) | exporter^year + importer^year + pair_id,
# #      data = trade_data
# #    )
# #
# # 5. DIFFERENT TRADE FLOW MEASURES:
# #    - tradeflow_comtrade_o: exports reported by origin
# #    - tradeflow_comtrade_d: imports reported by destination
# #    - tradeflow_imf_o/d: IMF DOTS data
# #    - Compare results across different data sources
# #
# # 6. EXPLORE OTHER VARIABLES IN THE DATASET:
# #    - WTO membership (gatt_o, wto_o)
# #    - EU membership (eu_o, eu_d)
# #    - Colonial relationships (col_dep, empire, sibling)
# #    - Legal origins (comleg_pretrans, comleg_posttrans)
# #    - Entry costs (entry_cost_o, entry_proc_o, entry_time_o)
# #
# # 7. PRODUCT-LEVEL ANALYSIS (requires BACI HS data):
# #    - Download BACI for product-level trade flows
# #    - Merge with this gravity dataset
# #    - Estimate gravity at HS6 product level
# #    - Use exporter^year^product, importer^year^product FEs
# #
# # 8. ROBUSTNESS CHECKS:
# #    - Exclude very small economies
# #    - Test different distance measures (distw_harmonic, distcap)
# #    - Use GDP PPP instead of nominal GDP
# #    - Bootstrap standard errors
# #    - Cluster standard errors by country-pair
# #
# # 9. POLICY COUNTERFACTUALS:
# #    - What if two countries sign an FTA?
# #    - Predict trade flows under different scenarios
# #    - Calculate welfare effects using structural parameters

# # Example: Focus on a specific policy question
# # "What was the impact of EU enlargement on trade?"

# eu_analysis <- gravity_full %>%
#   filter(
#     year >= 2000, year <= 2020,
#     !is.na(tradeflow_baci), tradeflow_baci > 0,
#     # At least one country in EU or joining EU
#     (eu_o == 1 | eu_d == 1),
#     iso3_o != iso3_d
#   ) %>%
#   select(year, iso3_o, iso3_d, tradeflow_baci, eu_o, eu_d, dist, gdp_o, gdp_d) %>%
#   mutate(both_eu = (eu_o == 1 & eu_d == 1))

# Model: Does joining EU increase trade?
# model_eu <- fepois(
#   tradeflow_baci ~ both_eu | iso3_o^year + iso3_d^year + iso3_o:iso3_d,
#   data = eu_analysis
# )

# ==============================================================================
# END OF SCRIPT
# ==============================================================================

# Save your workspace (optional)
# save.image("gravity_model_workspace.RData")

# To load later:
# load("gravity_model_workspace.RData")
