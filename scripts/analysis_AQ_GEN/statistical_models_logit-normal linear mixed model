library(dplyr)
library(readr)
library(lme4)
library(emmeans)

# ---------------------------------------------
# Load data
# ---------------------------------------------
df <- read_csv("fix_perc_ASQ_GEN.csv")

# ---------------------------------------------
# Filter: additional sample AND eye AOI
# ---------------------------------------------
df_eye <- df %>% 
  filter(ID >= 200, AOI == "eye_brow")

# Ensure Drug is a factor with correct order
df_eye$Drug <- factor(df_eye$Drug,
                      levels = c("placebo", "morphine", "naltrexone"))

# ---------------------------------------------
# Prepare dependent variable (logit-normal model)
# ---------------------------------------------
# Convert FixTimePerc (0–100) → proportion (0–1)
df_eye$prop <- df_eye$FixTimePerc / 100

# Avoid 0 and 1 (logit cannot handle them)
eps <- 1e-6
df_eye$prop_adj <- pmin(pmax(df_eye$prop, eps), 1 - eps)

# Logit transform
df_eye$logit_prop <- qlogis(df_eye$prop_adj)

# ---------------------------------------------
# Fit the linear mixed model (LMM)
# ---------------------------------------------
model_drug <- lmer(
  logit_prop ~ Drug + (1 | ID),
  data = df_eye
)

# Print model summary
print(summary(model_drug))

# ---------------------------------------------
# Post-hoc tests with emmeans
# ---------------------------------------------
emm_drug <- emmeans(model_drug, ~ Drug)

# Print estimated marginal means (on logit scale)
print(emm_drug)

# Pairwise comparisons (Morphine vs Placebo, Naltrexone vs Placebo)
contrast_drug <- pairs(emm_drug, adjust = "tukey")

# Print contrasts
print(contrast_drug)

# ---------------------------------------------
# Optional: back-transform means to proportions
# ---------------------------------------------
emm_back <- summary(emm_drug, type = "response")
print(emm_back)
