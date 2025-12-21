# Script to estimate gravity model using PPML
# Follows Herman (2023) methodology with three-way fixed effects
# Estimates separate models for each manufacturing sector
# Author: Created for gravity model project
# Date: 2025

library(tidyverse)
library(fixest)      # Fast PPML with high-dimensional fixed effects
library(here)
library(broom)

# Configuration ================================================================
TRADE_DATA_PATH <- "data/baci-agg"
RESULTS_PATH <- "results/gravity_estimates"
YEARS <- 2002:2022

# Industry codes (manufacturing D10-D32)
MANUFACTURING_INDUSTRIES <- c(
  "D10T12", "D13T15", "D16", "D17T18", "D19", "D20", "D21", "D22", "D23",
  "D24", "D25", "D26", "D27", "D28", "D29", "D30", "D31T33"
)

# Industry names for nice output
INDUSTRY_NAMES <- c(
  "D10T12" = "Food, beverages, tobacco",
  "D13T15" = "Textiles, apparel, leather",
  "D16" = "Wood products",
  "D17T18" = "Paper and printing",
  "D19" = "Coke and petroleum",
  "D20" = "Chemicals",
  "D21" = "Pharmaceuticals",
  "D22" = "Rubber and plastics",
  "D23" = "Non-metallic minerals",
  "D24" = "Basic metals",
  "D25" = "Fabricated metal products",
  "D26" = "Computer, electronic, optical",
  "D27" = "Electrical equipment",
  "D28" = "Machinery and equipment",
  "D29" = "Motor vehicles",
  "D30" = "Other transport equipment",
  "D31T33" = "Furniture and other manufacturing"
)

# Model specifications
# Following Herman (2023) Table 3, column (3) - PPML
MODEL_FORMULA <- "v ~ fta_wto + eu_both + contig + comlang_off + col_dep_ever | exporter_year + importer_year"
# Following Herman (2023) Table 3, column (4) - three-way gravity (including country-pair fixed effects)
MODEL_FORMULA <- "v ~ fta_wto + eu_both | exporter_year + importer_year + pair"

# Helper Functions =============================================================

#' Load all trade data
load_all_trade_data <- function(years = YEARS, path = TRADE_DATA_PATH) {
  
  cat("========================================\n")
  cat("LOADING TRADE DATA\n")
  cat("========================================\n\n")
  
  all_data <- list()
  
  for (year in years) {
    file_path <- file.path(
      path,
      paste0("BACI_HS02_Y", year, "_V202501_gravityready.csv")
    )
    
    if (!file.exists(file_path)) {
      cat("WARNING: File not found for year", year, "\n")
      next
    }
    
    cat("Loading year", year, "... ")
    data_year <- read_csv(file_path, show_col_types = FALSE)
    cat(nrow(data_year), "rows\n")
    
    all_data[[as.character(year)]] <- data_year
  }
  
  cat("\nCombining all years...\n")
  combined <- bind_rows(all_data)
  
  cat("✓ Total observations:", nrow(combined), "\n")
  cat("  Years:", paste(range(combined$t), collapse = " to "), "\n")
  cat("  Countries:", length(unique(c(combined$i, combined$j))), "\n")
  cat("  Industries:", length(unique(combined$k)), "\n")
  
  return(combined)
}

#' Prepare data for gravity estimation
prepare_gravity_data <- function(trade_data) {
  
  cat("\n========================================\n")
  cat("PREPARING DATA FOR ESTIMATION\n")
  cat("========================================\n\n")
  
  # Create necessary variables
  gravity_data <- trade_data %>%
    mutate(
      # Create fixed effect identifiers
      exporter_year = paste(i, t, sep = "_"),
      importer_year = paste(j, t, sep = "_"),
      pair = paste(pmin(i, j), pmax(i, j), sep = "_"),  # Symmetric pair
      
      # Create EU dummy (both countries in EU)
      eu_both = as.integer(eu_o == 1 & eu_d == 1),
      
      # Handle missing values in key variables
      fta_wto = replace_na(fta_wto, 0),
      contig = replace_na(contig, 0),
      comlang_off = replace_na(comlang_off, 0),
      col_dep_ever = replace_na(col_dep_ever, 0),
      
      # Log distance
      log_dist = log(distw_harmonic)
    )
  
  # Check data quality
  cat("--- Data Quality Checks ---\n")
  cat("Missing trade values:", sum(is.na(gravity_data$v)), "\n")
  cat("Zero trade flows:", sum(gravity_data$v == 0, na.rm = TRUE), "\n")
  cat("Negative trade flows:", sum(gravity_data$v < 0, na.rm = TRUE), "\n")
  
  # Summary of key variables
  cat("\n--- Key Variables Summary ---\n")
  cat("FTA pairs:", sum(gravity_data$fta_wto == 1, na.rm = TRUE),
      "(", round(100 * mean(gravity_data$fta_wto, na.rm = TRUE), 1), "%)\n")
  cat("EU pairs:", sum(gravity_data$eu_both == 1, na.rm = TRUE),
      "(", round(100 * mean(gravity_data$eu_both, na.rm = TRUE), 1), "%)\n")
  cat("Contiguous pairs:", sum(gravity_data$contig == 1, na.rm = TRUE),
      "(", round(100 * mean(gravity_data$contig, na.rm = TRUE), 1), "%)\n")
  cat("Common language:", sum(gravity_data$comlang_off == 1, na.rm = TRUE),
      "(", round(100 * mean(gravity_data$comlang_off, na.rm = TRUE), 1), "%)\n")
  cat("Colonial ties:", sum(gravity_data$col_dep_ever == 1, na.rm = TRUE),
      "(", round(100 * mean(gravity_data$col_dep_ever, na.rm = TRUE), 1), "%)\n")
  
  cat("\n--- Fixed Effects Dimensions ---\n")
  cat("Exporter-year combinations:", length(unique(gravity_data$exporter_year)), "\n")
  cat("Importer-year combinations:", length(unique(gravity_data$importer_year)), "\n")
  cat("Country pairs:", length(unique(gravity_data$pair)), "\n")
  
  return(gravity_data)
}

#' Estimate gravity model for one sector
estimate_sector <- function(sector_code, data, formula = MODEL_FORMULA) {
  
  cat("\n=== Estimating:", sector_code, "-", INDUSTRY_NAMES[sector_code], "===\n")
  
  # Filter data for this sector
  sector_data <- data %>%
    filter(k == sector_code)
  
  cat("Observations:", nrow(sector_data), "\n")
  cat("Non-zero flows:", sum(sector_data$v > 0), "\n")
  cat("Zero flows:", sum(sector_data$v == 0), "\n")
  
  if (nrow(sector_data) < 100) {
    cat("WARNING: Too few observations. Skipping.\n")
    return(NULL)
  }
  
  # Estimate PPML model
  cat("Estimating PPML with three-way fixed effects...\n")
  
  model <- tryCatch({
    feglm(
      as.formula(formula),
      data = sector_data,
      family = "poisson",
      vcov = ~pair  # Cluster standard errors by country pair
    )
  }, error = function(e) {
    cat("ERROR in estimation:", e$message, "\n")
    return(NULL)
  })
  
  if (is.null(model)) {
    return(NULL)
  }
  
  # Report results
  cat("✓ Estimation successful\n")
  cat("Observations used:", nobs(model), "\n")
  
  # Extract and display coefficients
  coef_summary <- summary(model)$coefficients
  cat("\nCoefficients:\n")
  print(coef_summary)
  
  return(model)
}

#' Estimate all sectors
estimate_all_sectors <- function(data, sectors = MANUFACTURING_INDUSTRIES) {
  
  cat("\n========================================\n")
  cat("ESTIMATING GRAVITY MODELS BY SECTOR\n")
  cat("========================================\n")
  
  results <- list()
  
  for (sector in sectors) {
    results[[sector]] <- estimate_sector(sector, data)
    
    # Save intermediate results
    if (!is.null(results[[sector]])) {
      saveRDS(
        results[[sector]],
        file.path(RESULTS_PATH, paste0("model_", sector, ".rds"))
      )
    }
  }
  
  # Count successful estimations
  successful <- sum(!map_lgl(results, is.null))
  cat("\n========================================\n")
  cat("Estimation Summary\n")
  cat("========================================\n")
  cat("Successfully estimated:", successful, "of", length(sectors), "sectors\n")
  
  return(results)
}

#' Aggregate model (pooled across sectors)
estimate_aggregate_model <- function(data, formula = MODEL_FORMULA) {
  
  cat("\n========================================\n")
  cat("ESTIMATING AGGREGATE MODEL\n")
  cat("========================================\n\n")
  
  cat("Pooling all manufacturing sectors...\n")
  cat("Total observations:", nrow(data), "\n")
  
  # Estimate
  cat("Estimating PPML...\n")
  model_agg <- feglm(
    as.formula(formula),
    data = data,
    family = "poisson",
    vcov = ~pair
  )
  
  cat("✓ Estimation successful\n")
  cat("Observations used:", nobs(model_agg), "\n\n")
  
  # Display results
  print(summary(model_agg))
  
  return(model_agg)
}

#' Create results table
create_results_table <- function(models) {
  
  cat("\n========================================\n")
  cat("CREATING RESULTS TABLE\n")
  cat("========================================\n\n")
  
  # Extract coefficients from each model
  results_list <- list()
  
  for (sector in names(models)) {
    if (is.null(models[[sector]])) next
    
    coefs <- broom::tidy(models[[sector]]) %>%
      mutate(
        sector = sector,
        industry = INDUSTRY_NAMES[sector]
      ) %>%
      select(sector, industry, term, estimate, std.error, statistic, p.value)
    
    results_list[[sector]] <- coefs
  }
  
  results_table <- bind_rows(results_list)
  
  # Wide format for easy comparison
  results_wide <- results_table %>%
    select(sector, industry, term, estimate, p.value) %>%
    mutate(
      significance = case_when(
        p.value < 0.01 ~ "***",
        p.value < 0.05 ~ "**",
        p.value < 0.10 ~ "*",
        TRUE ~ ""
      ),
      estimate_fmt = paste0(
        round(estimate, 3),
        significance
      )
    ) %>%
    select(sector, industry, term, estimate_fmt) %>%
    pivot_wider(
      names_from = term,
      values_from = estimate_fmt
    )
  
  return(list(
    long = results_table,
    wide = results_wide
  ))
}

#' Export results
export_results <- function(models, results_table, agg_model = NULL) {
  
  cat("\n========================================\n")
  cat("EXPORTING RESULTS\n")
  cat("========================================\n\n")
  
  if (!dir.exists(RESULTS_PATH)) {
    dir.create(RESULTS_PATH, recursive = TRUE)
  }
  
  # Save detailed results (long format)
  write_csv(
    results_table$long,
    file.path(RESULTS_PATH, "gravity_estimates_detailed.csv")
  )
  cat("✓ Saved: gravity_estimates_detailed.csv\n")
  
  # Save summary table (wide format)
  write_csv(
    results_table$wide,
    file.path(RESULTS_PATH, "gravity_estimates_summary.csv")
  )
  cat("✓ Saved: gravity_estimates_summary.csv\n")
  
  # Save aggregate model
  if (!is.null(agg_model)) {
    saveRDS(
      agg_model,
      file.path(RESULTS_PATH, "model_aggregate.rds")
    )
    
    # Export aggregate coefficients
    agg_coefs <- broom::tidy(agg_model) %>%
      mutate(
        significance = case_when(
          p.value < 0.01 ~ "***",
          p.value < 0.05 ~ "**",
          p.value < 0.10 ~ "*",
          TRUE ~ ""
        )
      )
    
    write_csv(
      agg_coefs,
      file.path(RESULTS_PATH, "gravity_estimates_aggregate.csv")
    )
    cat("✓ Saved: gravity_estimates_aggregate.csv\n")
  }
  
  # Save model objects
  saveRDS(
    models,
    file.path(RESULTS_PATH, "all_models.rds")
  )
  cat("✓ Saved: all_models.rds\n")
  
  cat("\n")
}

#' Create visualization of estimates
visualize_estimates <- function(results_table) {
  
  cat("========================================\n")
  cat("CREATING VISUALIZATIONS\n")
  cat("========================================\n\n")
  
  # Filter to key variables
  key_vars <- c("fta_wto", "eu_both", "contig", "comlang_off", "col_dep_ever")
  
  plot_data <- results_table$long %>%
    filter(term %in% key_vars) %>%
    mutate(
      term_label = recode(
        term,
        "fta_wto" = "Free Trade Agreement",
        "eu_both" = "Both in EU",
        "contig" = "Contiguous",
        "comlang_off" = "Common Language",
        "col_dep_ever" = "Colonial Ties"
      )
    )
  
  # Plot 1: Coefficient estimates by sector
  p1 <- ggplot(plot_data, aes(x = industry, y = estimate, color = term_label)) +
    geom_point(size = 3) +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * std.error,
          ymax = estimate + 1.96 * std.error),
      width = 0.2
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    facet_wrap(~term_label, scales = "free_y", ncol = 2) +
    coord_flip() +
    labs(
      title = "Gravity Model Estimates by Manufacturing Sector",
      subtitle = "PPML estimation with three-way fixed effects",
      x = "Industry",
      y = "Coefficient Estimate (95% CI)",
      caption = "Data: BACI trade flows 2002-2022, USITC Dynamic Gravity Dataset"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 8)
    )
  
  ggsave(
    file.path(RESULTS_PATH, "gravity_estimates_by_sector.png"),
    p1,
    width = 12,
    height = 10
  )
  cat("✓ Saved: gravity_estimates_by_sector.png\n")
  
  # Plot 2: Distribution of FTA effects
  p2 <- plot_data %>%
    filter(term == "fta_wto") %>%
    ggplot(aes(x = reorder(industry, estimate), y = estimate)) +
    geom_col(aes(fill = estimate > 0)) +
    geom_errorbar(
      aes(ymin = estimate - 1.96 * std.error,
          ymax = estimate + 1.96 * std.error),
      width = 0.3
    ) +
    coord_flip() +
    scale_fill_manual(values = c("red", "darkgreen")) +
    labs(
      title = "Effect of Free Trade Agreements on Trade",
      subtitle = "By manufacturing sector",
      x = "Industry",
      y = "Coefficient Estimate",
      caption = "Positive values indicate trade-creating effect"
    ) +
    theme_minimal() +
    theme(legend.position = "none")
  
  ggsave(
    file.path(RESULTS_PATH, "fta_effects_by_sector.png"),
    p2,
    width = 10,
    height = 8
  )
  cat("✓ Saved: fta_effects_by_sector.png\n")
  
  cat("\n")
}

# Main Execution ===============================================================

main <- function() {
  
  cat("========================================\n")
  cat("GRAVITY MODEL ESTIMATION\n")
  cat("PPML with Three-Way Fixed Effects\n")
  cat("========================================\n\n")
  
  # Create results directory
  if (!dir.exists(RESULTS_PATH)) {
    dir.create(RESULTS_PATH, recursive = TRUE)
  }
  
  # Step 1: Load all trade data
  trade_data <- load_all_trade_data()
  
  # Step 2: Prepare data
  gravity_data <- prepare_gravity_data(trade_data)
  
  # Step 3: Estimate sector-by-sector
  sector_models <- estimate_all_sectors(gravity_data)
  
  # Step 4: Estimate aggregate model
  agg_model <- estimate_aggregate_model(gravity_data)
  
  # Step 5: Create results table
  results_table <- create_results_table(sector_models)
  
  # Step 6: Display results
  cat("\n========================================\n")
  cat("RESULTS SUMMARY\n")
  cat("========================================\n\n")
  print(results_table$wide)
  
  # Step 7: Export results
  export_results(sector_models, results_table, agg_model)
  
  # Step 8: Visualize
  tryCatch({
    visualize_estimates(results_table)
  }, error = function(e) {
    cat("Note: Could not create visualizations:", e$message, "\n")
  })
  
  cat("\n========================================\n")
  cat("ESTIMATION COMPLETE\n")
  cat("========================================\n")
  cat("\nResults saved in:", RESULTS_PATH, "\n")
  cat("\nNext steps:\n")
  cat("1. Review coefficient estimates\n")
  cat("2. Check for unexpected results\n")
  cat("3. Proceed to counterfactual analysis\n\n")
  
  return(invisible(list(
    sector_models = sector_models,
    aggregate_model = agg_model,
    results = results_table
  )))
}

# Run
if (interactive()) {
  cat("Script loaded. Run main() to execute.\n")
} else {
  results <- main()
}