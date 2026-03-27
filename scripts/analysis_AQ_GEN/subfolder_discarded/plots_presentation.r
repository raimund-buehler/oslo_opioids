# ============================================================
# Barplot (Drug) with 95% CI from saved emmeans (glmmTMB, beta)
# - Uses RDS posthoc file directly (no refit needed)
# - No split by FaceGender
# - Y axis = Fixation Time (%)
# - Colors: morphine #4ef59c, placebo gray, naltrexone #c21345
# ============================================================

library(tidyverse)
library(emmeans)
library(here)
library(readr)

# -----------------------
# Load saved posthoc RDS
# -----------------------
posthoc <- readRDS(here("models", "posthoc_results_slope.rds"))

# Model 1: emmeans object returned by emmeans(model1, pairwise ~ Drug)
m1_drug_pw <- posthoc$Model1$drug_contrasts

# Extract the EMMs part
emm_obj <- if (is.list(m1_drug_pw) && !is.null(m1_drug_pw$emmeans)) m1_drug_pw$emmeans else m1_drug_pw

# Summarize on response scale (proportion), incl CI
emm_sum <- as.data.frame(summary(emm_obj, type = "response", infer = TRUE))

# Find CI column names robustly (df may be disabled -> asymp.LCL/UCL)
lower_col <- intersect(c("lower.CL", "asymp.LCL", "lower.HPD"), names(emm_sum))[1]
upper_col <- intersect(c("upper.CL", "asymp.UCL", "upper.HPD"), names(emm_sum))[1]
if (is.na(lower_col) || is.na(upper_col)) {
  stop("Could not find CI columns in emmeans summary. Columns are: ",
       paste(names(emm_sum), collapse = ", "))
}

# Build plot dataframe:
# - emmean is on response scale (FixProp) -> convert to percent
plot_dat <- emm_sum %>%
  transmute(
    Drug = factor(Drug, levels = c("placebo", "morphine", "naltrexone")),
    FixTimePerc = response * 100,
    lower = .data[[lower_col]] * 100,
    upper = .data[[upper_col]] * 100
  )

# Shared y-limits (based on CI range)
ylims <- range(c(plot_dat$lower, plot_dat$upper), na.rm = TRUE)

# Colors
drug_cols <- c(
  morphine   = "#4ef59c",
  placebo    = "gray70",
  naltrexone = "#c21345"
)

# -----------------------
# Plot
# -----------------------
p_drug <- ggplot(plot_dat, aes(x = Drug, y = FixTimePerc, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  coord_cartesian(ylim = ylims) +
  labs(
    x = NULL,
    y = "Fixation Time (%)",
    title = "Estimated fixation time in eye region by Drug (Model 1, additional dataset)",
    subtitle = "Bars = emmeans on response scale; error bars = 95% CI"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_drug

# -----------------------
# Save
# -----------------------
out_dir <- here("results", "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "fixation_eye_emm_drug_model1_additional_slope.png"),
       p_drug, width = 7, height = 5, dpi = 300)

cat("Saved plot to:\n", out_dir, "\n")

#H3: Drug by ASQ interaction (Model 2)

# -----------------------
# Load saved posthoc RDS
# -----------------------
posthoc <- readRDS(here("models", "posthoc_results_slope.rds"))

m2_low  <- posthoc$Model2$drug_low_asq   # emmeans(model2, pairwise ~ Drug | ASQ, at=low)
m2_high <- posthoc$Model2$drug_high_asq  # emmeans(model2, pairwise ~ Drug | ASQ, at=high)

# Helper: pull emmeans part from an emmeans "pairwise" list
get_emm_part <- function(x) {
  if (is.list(x) && !is.null(x$emmeans)) x$emmeans else x
}

# Helper: robust CI columns from emmeans summary
emm_to_plotdf <- function(emm, asq_label) {
  s <- as.data.frame(summary(emm, type = "response", infer = TRUE))

  lower_col <- intersect(c("lower.CL", "asymp.LCL", "lower.HPD"), names(s))[1]
  upper_col <- intersect(c("upper.CL", "asymp.UCL", "upper.HPD"), names(s))[1]
  if (is.na(lower_col) || is.na(upper_col)) {
    stop("Could not find CI columns. Available: ", paste(names(s), collapse = ", "))
  }

  s %>%
    transmute(
      ASQ_level = asq_label,
      Drug = factor(Drug, levels = c("placebo", "morphine", "naltrexone")),
      FixTimePerc = response * 100,
      lower = .data[[lower_col]] * 100,
      upper = .data[[upper_col]] * 100
    )
}

plot_low  <- emm_to_plotdf(get_emm_part(m2_low),  "Low ASQ (25th pct)")
plot_high <- emm_to_plotdf(get_emm_part(m2_high), "High ASQ (75th pct)")

plot_dat <- bind_rows(plot_low, plot_high) %>%
  mutate(ASQ_level = factor(ASQ_level, levels = c("Low ASQ (25th pct)", "High ASQ (75th pct)")))

# Shared y-limits across panels
ylims <- range(c(plot_dat$lower, plot_dat$upper), na.rm = TRUE)

# Drug colors (as you used before)
drug_cols <- c(
  morphine   = "#4ef59c",
  placebo    = "gray70",
  naltrexone = "#c21345"
)

# -----------------------
# Add annotation for the significant post-hoc at HIGH ASQ:
# morphine vs naltrexone OR = 2.006, p = .010
# -----------------------
y_annot <- plot_dat %>%
  filter(ASQ_level == "High ASQ (75th pct)") %>%
  summarise(y = max(upper, na.rm = TRUE)) %>%
  pull(y)

annot_df <- tibble(
  ASQ_level = factor("High ASQ (75th pct)", levels = levels(plot_dat$ASQ_level)),
  x1 = "morphine",
  x2 = "naltrexone",
  y  = y_annot + 0.03 * diff(ylims),
  label = "M vs N: OR = 2.006, p = .010"
)

# -----------------------
# Plot
# -----------------------
p_h3 <- ggplot(plot_dat, aes(x = Drug, y = FixTimePerc, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  facet_wrap(~ ASQ_level) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  coord_cartesian(
    ylim = c(ylims[1], max(ylims[2], annot_df$y) + 0.08 * diff(ylims)),
    clip = "off"                 # <-- important: allow drawing outside panel if needed
  ) +
  labs(
    x = NULL,
    y = "Fixation Time (%)",
    title = "Drug effects at low vs high ASQ (Model 2: Drug × ASQ)",
    subtitle = "Bars = emmeans on response scale; error bars = 95% CI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 20, 10, 20)  # <-- a bit more room
  ) +
  geom_segment(
    data = annot_df,
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_segment(
    data = annot_df,
    aes(x = x1, xend = x1, y = y, yend = y - 0.01 * diff(ylims)),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_segment(
    data = annot_df,
    aes(x = x2, xend = x2, y = y, yend = y - 0.01 * diff(ylims)),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_text(
    data = annot_df,
    aes(x = "morphine", y = y + 0.02 * diff(ylims), label = label),  # <-- moved to center
    inherit.aes = FALSE,
    hjust = 0.5,
    size = 3.5
  )


p_h3

# -----------------------
# Save
# -----------------------
out_dir <- here("results", "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "model2_drug_by_asq_low_high_barplot.png"), p_h3, width = 8, height = 5, dpi = 300)

cat("Saved plot to:\n", out_dir, "\n")


# ============================================================
# EH4.1 plot using MODEL 1 (Genetics main effect)
# - Barplot of predicted fixation time (%) for Genetics (A vs G)
# - 95% CI
# - Uses saved RDS (no refit): models/all_glmm_models_slope.rds
# - Colors: A = #F8766D (orange), G = #00BFC4 (mint-blue)
# ============================================================

library(tidyverse)
library(emmeans)
library(here)

# -----------------------
# Load model1 from saved RDS
# -----------------------
models <- readRDS(here("models", "all_glmm_models_slope.rds"))
model1 <- models$model1

# -----------------------
# Fix factor types in model frame (avoids "contrasts apply only to factors")
# -----------------------
mf <- model1$frame
fac_vars <- c("Drug", "Genetics", "FaceGender", "Gaze2", "AttrLevel", "Imagelist", "Session", "ID")
for (v in fac_vars) {
  if (v %in% names(mf) && !is.factor(mf[[v]])) mf[[v]] <- factor(mf[[v]])
}

# Optional: stable ordering
if ("Genetics" %in% names(mf)) mf$Genetics <- factor(mf$Genetics, levels = c("A", "G"))
if ("Drug" %in% names(mf))     mf$Drug     <- factor(mf$Drug, levels = c("placebo","morphine","naltrexone"))

# Nuisance factors: average over these so the reference grid stays small
nuis <- c("Drug", "FaceGender", "Gaze2", "AttrLevel", "Imagelist", "Session")

# -----------------------
# Get EMMs for Genetics on response scale
# (averaged over nuisance factors; ASQ held at its mean in the model frame)
# -----------------------
ASQ_mean <- mean(mf$ASQ, na.rm = TRUE)

rg <- ref_grid(
  model1,
  data = mf,
  at = list(ASQ = ASQ_mean),
  nuisance = nuis,
  type = "response"
)

emm_gen <- emmeans(rg, ~ Genetics)

emm_df <- as.data.frame(summary(emm_gen, infer = TRUE, type = "response"))

# robust CI column names
lower_col <- intersect(c("lower.CL", "asymp.LCL", "lower.HPD"), names(emm_df))[1]
upper_col <- intersect(c("upper.CL", "asymp.UCL", "upper.HPD"), names(emm_df))[1]
if (is.na(lower_col) || is.na(upper_col)) {
  stop("Could not find CI columns. Available: ", paste(names(emm_df), collapse = ", "))
}

plot_dat <- emm_df %>%
  transmute(
    Genetics = factor(Genetics, levels = c("A","G")),
    FixTimePerc = response * 100,
    lower = .data[[lower_col]] * 100,
    upper = .data[[upper_col]] * 100
  )

ylims <- range(c(plot_dat$lower, plot_dat$upper), na.rm = TRUE)

# Colors (match your screenshot)
gen_cols <- c(
  A = "#F8766D",
  G = "#00BFC4"
)

# -----------------------
# Plot
# -----------------------
p_EH41 <- ggplot(plot_dat, aes(x = Genetics, y = FixTimePerc, fill = Genetics)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  scale_fill_manual(values = gen_cols, drop = FALSE) +
  coord_cartesian(ylim = c(ylims[1], ylims[2] + 0.12 * diff(ylims)), clip = "off") +
  labs(
    x = "Genotype",
    y = "Fixation Time (%)",
    title = "EH4.1 (Model 1): Eye-region fixation by genotype",
    subtitle = "Bars = predicted means on response scale (ASQ at mean); error bars = 95% CI"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_EH41

# -----------------------
# Save
# -----------------------
out_dir <- here("results", "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "model1_genotype_fixation_EH4_1.png"), p_EH41, width = 7, height = 5, dpi = 300)


cat("Saved plot to:\n", out_dir, "\n")


# ============================================================
# Plots for EH4.2 (Model 3): Genetics effect + ASQ trend by Genetics
# - Panel A: Predicted fixation (%) vs ASQ for each genotype (lines + 95% CI ribbon)
# - Panel B: Genotype difference at mean ASQ (bar + 95% CI) + annotation
# Uses saved RDS: models/posthoc_results_slope.rds (m3_slopes)
# ============================================================



# - avoids emmeans rg.limit explosion via nuisance factors
# - avoids "contrasts apply only to factors" by forcing factor columns in model frame
# - uses colors: A = #F8766D (orange), G = #00BFC4 (mint-blue)
# - CI ribbons are same color with alpha transparency
# ============================================================

library(tidyverse)
library(emmeans)
library(here)
library(readr)

# -----------------------
# Load model3 from saved RDS
# -----------------------
models <- readRDS(here("models", "all_glmm_models_slope.rds"))
model3 <- models$model3

# -----------------------
# Load data (only to get ASQ range + mean)
# -----------------------
df <- read_csv(here("data", "analyses", "fix_perc_ASQ_GEN.csv"))
df_eye <- df %>% filter(AOI == "eye_brow")

ASQ_min  <- min(df_eye$ASQ, na.rm = TRUE)
ASQ_max  <- max(df_eye$ASQ, na.rm = TRUE)
ASQ_mean <- mean(df_eye$ASQ, na.rm = TRUE)
asq_grid <- seq(ASQ_min, ASQ_max, length.out = 50)

# -----------------------
# Helper: robust CI column names
# -----------------------
get_ci_cols <- function(df) {
  lower_col <- intersect(c("lower.CL", "asymp.LCL", "lower.HPD"), names(df))[1]
  upper_col <- intersect(c("upper.CL", "asymp.UCL", "upper.HPD"), names(df))[1]
  if (is.na(lower_col) || is.na(upper_col)) {
    stop("Could not find CI columns. Available: ", paste(names(df), collapse = ", "))
  }
  list(lower = lower_col, upper = upper_col)
}

# -----------------------
# IMPORTANT FIX:
# Ensure factor columns are factors in model frame for ref_grid()
# -----------------------
mf <- model3$frame

fac_vars <- c("Drug", "Genetics", "FaceGender", "Gaze2", "AttrLevel", "Imagelist", "Session", "ID")
for (v in fac_vars) {
  if (v %in% names(mf) && !is.factor(mf[[v]])) mf[[v]] <- factor(mf[[v]])
}

# Optional: stable ordering (recommended)
if ("Drug" %in% names(mf)) {
  mf$Drug <- factor(mf$Drug, levels = c("placebo", "morphine", "naltrexone"))
}
if ("Genetics" %in% names(mf)) {
  mf$Genetics <- factor(mf$Genetics, levels = c("A", "G"))
}

# Nuisance factors: average over these to keep reference grid small
nuis <- c("Drug", "FaceGender", "Gaze2", "AttrLevel", "Imagelist", "Session")

# -----------------------
# Color coding (match your screenshot / ggplot default discrete colors)
# -----------------------
gen_cols <- c(
  A = "#F8766D",   # orange/salmon
  G = "#00BFC4"    # mint-blue/teal
)

# ============================================================
# Plot 1: Interaction plot (Fixation % across ASQ by genotype)
# ============================================================
rg_A <- ref_grid(
  model3,
  data = mf,
  at = list(ASQ = asq_grid),
  nuisance = nuis,
  type = "response"
)

emm_asq_by_gen <- emmeans(rg_A, ~ Genetics | ASQ)
emm_df <- as.data.frame(summary(emm_asq_by_gen, infer = TRUE, type = "response"))
ci <- get_ci_cols(emm_df)

plot_lines <- emm_df %>%
  transmute(
    ASQ,
    Genetics,
    FixTimePerc = response * 100,
    lower = .data[[ci$lower]] * 100,
    upper = .data[[ci$upper]] * 100
  )

p_A <- ggplot(plot_lines, aes(x = ASQ, y = FixTimePerc, color = Genetics, fill = Genetics, group = Genetics)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, colour = NA) +
  geom_line(linewidth = 1) +
  scale_color_manual(values = gen_cols, drop = FALSE) +
  scale_fill_manual(values = gen_cols, drop = FALSE) +
  labs(
    x = "ASQ",
    y = "Fixation Time (%)",
    title = "Model 3: Predicted fixation across ASQ by genotype",
    subtitle = "Lines = predicted means; ribbons = 95% CI"
  ) +
  theme_minimal(base_size = 12)

# ============================================================
# Plot 2: Barplot (Genotype difference at mean ASQ)
# ============================================================
rg_B <- ref_grid(
  model3,
  data = mf,
  at = list(ASQ = ASQ_mean),
  nuisance = nuis,
  type = "response"
)

emm_gen_at_mean_asq <- emmeans(rg_B, ~ Genetics)
gen_df <- as.data.frame(summary(emm_gen_at_mean_asq, infer = TRUE, type = "response"))
ci2 <- get_ci_cols(gen_df)

plot_bars <- gen_df %>%
  transmute(
    Genetics,
    FixTimePerc = response * 100,
    lower = .data[[ci2$lower]] * 100,
    upper = .data[[ci2$upper]] * 100
  )

ylims_b <- range(c(plot_bars$lower, plot_bars$upper), na.rm = TRUE)

p_B <- ggplot(plot_bars, aes(x = Genetics, y = FixTimePerc, fill = Genetics)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  scale_fill_manual(values = gen_cols, drop = FALSE) +
  coord_cartesian(
    ylim = c(ylims_b[1], ylims_b[2] + 0.18 * diff(ylims_b)),
    clip = "off"
  ) +
  labs(
    x = "Genotype",
    y = "Fixation Time (%)",
    title = "Genotype difference (at mean ASQ)",
    subtitle = "Bars = predicted means; error bars = 95% CI"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none") +
  annotate(
    "text",
    x = 1.5,
    y = ylims_b[2] + 0.12 * diff(ylims_b),
    label = "Genotype effect: χ²(1) = 5.26, p = .022\nOpposite direction: b = −0.731, SE = 0.319, z = −2.29, p = .022\nTukey OR(A/G) = 2.08, p = .022",
    size = 3.5
  )

# Show plots
p_A
p_B

# -----------------------
# Save plots
# -----------------------
out_dir <- here("results", "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "model3_fixation_by_asq_by_genotype.png"), p_A, width = 8, height = 5, dpi = 300)
ggsave(file.path(out_dir, "model3_genotype_difference_at_mean_asq.png"), p_B, width = 8, height = 5, dpi = 300)

cat("Saved plots to:\n", out_dir, "\n")

# -----------------------
# Load model5 (saved)
# -----------------------
models <- readRDS(here("models", "all_glmm_models_slope.rds"))
model5 <- models$model5

# -----------------------
# Load data only to set ASQ reference value (mean)
# -----------------------
df <- read_csv(here("data", "analyses", "fix_perc_ASQ_GEN.csv"))
df_eye <- df %>% filter(AOI == "eye_brow")
ASQ_mean <- mean(df_eye$ASQ, na.rm = TRUE)

# -----------------------
# Fix factor types in model frame (avoids "contrasts apply only to factors")
# -----------------------
mf <- model5$frame
fac_vars <- c("Drug", "Genetics", "FaceGender", "Gaze2", "AttrLevel", "Imagelist", "Session", "ID")
for (v in fac_vars) {
  if (v %in% names(mf) && !is.factor(mf[[v]])) mf[[v]] <- factor(mf[[v]])
}
if ("Drug" %in% names(mf))     mf$Drug     <- factor(mf$Drug, levels = c("placebo","morphine","naltrexone"))
if ("Genetics" %in% names(mf)) mf$Genetics <- factor(mf$Genetics, levels = c("A","G"))

# average over these to keep ref grid small + match your earlier reporting style
nuis <- c("FaceGender", "Gaze2", "AttrLevel", "Imagelist", "Session")

# -----------------------
# Build reference grid at ASQ = mean, then EMMs: Drug within Genetics
# -----------------------
rg <- ref_grid(
  model5,
  data = mf,
  at = list(ASQ = ASQ_mean),
  nuisance = nuis,
  type = "response"
)

emm_drug_by_gen <- emmeans(rg, ~ Drug | Genetics)
emm_df <- as.data.frame(summary(emm_drug_by_gen, infer = TRUE, type = "response"))

# Robust CI column names (df disabled -> asymp.LCL/UCL)
lower_col <- intersect(c("lower.CL", "asymp.LCL", "lower.HPD"), names(emm_df))[1]
upper_col <- intersect(c("upper.CL", "asymp.UCL", "upper.HPD"), names(emm_df))[1]
if (is.na(lower_col) || is.na(upper_col)) {
  stop("Could not find CI columns. Available: ", paste(names(emm_df), collapse = ", "))
}

plot_dat <- emm_df %>%
  transmute(
    Genetics = factor(Genetics, levels = c("A","G")),
    Drug = factor(Drug, levels = c("placebo","morphine","naltrexone")),
    FixTimePerc = response * 100,
    lower = .data[[lower_col]] * 100,
    upper = .data[[upper_col]] * 100
  )

ylims <- range(c(plot_dat$lower, plot_dat$upper), na.rm = TRUE)

# -----------------------
# Colors (as you used before)
# -----------------------
drug_cols <- c(
  morphine   = "#4ef59c",
  placebo    = "gray70",
  naltrexone = "#c21345"
)

# -----------------------
# Annotation ONLY in Genetics = G facet
# -----------------------
# Put the bracket a bit above the highest CI in the G facet
yG <- plot_dat %>%
  filter(Genetics == "G") %>%
  summarise(y = max(upper, na.rm = TRUE)) %>%
  pull(y)

annot_df <- tibble(
  Genetics = factor("G", levels = levels(plot_dat$Genetics)),
  x1 = "placebo",
  x2 = "morphine",
  y  = yG + 0.05 * diff(ylims),
  label = "placebo vs morphine: OR = 0.569, p = .017"
)

# -----------------------
# Plot
# -----------------------
p_simple <- ggplot(plot_dat, aes(x = Drug, y = FixTimePerc, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  facet_wrap(~ Genetics) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  coord_cartesian(
    ylim = c(ylims[1], max(ylims[2], annot_df$y) + 0.10 * diff(ylims)),
    clip = "off"
  ) +
  labs(
    x = NULL,
    y = "Fixation Time (%)",
    title = "Simple effects: Drug differences within genotype (ASQ at mean, Model 5)",
    subtitle = "Bars = predicted means on response scale; error bars = 95% CI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 20, 10, 20)
  ) +
  # bracket in Genetics = G only
  geom_segment(
    data = annot_df,
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_segment(
    data = annot_df,
    aes(x = x1, xend = x1, y = y, yend = y - 0.015 * diff(ylims)),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_segment(
    data = annot_df,
    aes(x = x2, xend = x2, y = y, yend = y - 0.015 * diff(ylims)),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_text(
    data = annot_df,
    aes(x = "morphine", y = y + 0.03 * diff(ylims), label = label),
    inherit.aes = FALSE,
    hjust = 0.5,
    size = 3.5
  )

p_simple

# -----------------------
# Save
# -----------------------
out_dir <- here("results", "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "model5_simple_effects_drug_within_genotype_ASQmean.png"),
       p_simple, width = 9, height = 5, dpi = 300)

ggsave(file.path(out_dir, "model5_simple_effects_drug_within_genotype_ASQmean.pdf"),
       p_simple, width = 9, height = 5)

cat("Saved plot to:\n", out_dir, "\n")