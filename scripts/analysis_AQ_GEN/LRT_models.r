library(glmmTMB)

# ----------------------------------------------------------
# Load models
# ----------------------------------------------------------
models_intercepts <- readRDS("models/fix_perc_ASQ_GEN/all_glmm_models_intercept.rds")
models_slopes     <- readRDS("models/fix_perc_ASQ_GEN/all_glmm_models_slope.rds")

# ----------------------------------------------------------
# PART 1: LRT between intercept-only vs. slope models
# OK for Model 1 because it compares models fit to the same dataset (within that model set)
# ----------------------------------------------------------
cat("\n==============================\n")
cat("LRT: Intercept-only vs Slope Models (Models 1–5)\n")
cat("==============================\n\n")

LRT_intercept_vs_slope <- list()

for (i in 1:5) {
  intercept_model <- models_intercepts[[i]]
  slope_model     <- models_slopes[[i]]

  lrt_result <- anova(intercept_model, slope_model)
  LRT_intercept_vs_slope[[paste0("Model", i)]] <- lrt_result

  cat(paste0("\n--- LRT Model ", i, " (Intercept vs Slope) ---\n"))
  print(lrt_result)
}

# ----------------------------------------------------------
# PART 2: LRT among slope models only
# SKIP comparisons involving Model 1 (1 vs 2) because different datasets
# ----------------------------------------------------------
cat("\n==============================\n")
cat("LRT: Comparisons Among Slope Models (2 vs 3, 3 vs 4, 4 vs 5)\n")
cat("==============================\n\n")

LRT_within_slopes <- list()

for (i in 2:4) {
  m1 <- models_slopes[[i]]
  m2 <- models_slopes[[i + 1]]

  lrt_result <- anova(m1, m2)
  name <- paste0("Slope_Model", i, "_vs_", i + 1)
  LRT_within_slopes[[name]] <- lrt_result

  cat(paste0("\n--- LRT Slope Models ", i, " vs ", i + 1, " ---\n"))
  print(lrt_result)
}

# ----------------------------------------------------------
# PART 3: LRT among intercept models only
# SKIP comparisons involving Model 1 (1 vs 2) because different datasets
# ----------------------------------------------------------
cat("\n==============================\n")
cat("LRT: Comparisons Among Intercept Models (2 vs 3, 3 vs 4, 4 vs 5)\n")
cat("==============================\n\n")

LRT_within_intercepts <- list()

for (i in 2:4) {
  m1 <- models_intercepts[[i]]
  m2 <- models_intercepts[[i + 1]]

  lrt_result <- anova(m1, m2)
  name <- paste0("Intercept_Model", i, "_vs_", i + 1)
  LRT_within_intercepts[[name]] <- lrt_result

  cat(paste0("\n--- LRT Intercept Models ", i, " vs ", i + 1, " ---\n"))
  print(lrt_result)
}

# ----------------------------------------------------------
# Combine and save results
# ----------------------------------------------------------
LRT_results <- list(
  intercept_vs_slope = LRT_intercept_vs_slope,
  within_slopes = LRT_within_slopes,
  within_intercepts = LRT_within_intercepts
)

saveRDS(LRT_results, file = "models/LRT_results.rds")

cat("\n\nAll LRT results saved in: models/LRT_results.rds\n")
cat("Note: Model-to-model comparisons involving Model 1 were skipped (different dataset).\n")
cat("      Intercept vs slope LRT for Model 1 was kept (within-model comparison).\n")
