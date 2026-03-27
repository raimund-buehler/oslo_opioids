library(tidyverse)
library(emmeans)
library(here)
library(readr)

# ============================================================
# Helper: force bar charts to start at 0 and keep top headroom
# ============================================================
ylim0 <- function(upper_vec, extra_top = 0.10) {
  ymax <- max(upper_vec, na.rm = TRUE)
  c(0, ymax + extra_top * ymax)
}

# ============================================================
# MODEL 1 Drug plot (from saved posthoc RDS)
# ============================================================

posthoc <- readRDS(here("models", "posthoc_results_slope.rds"))

m1_drug_pw <- posthoc$Model1$drug_contrasts
emm_obj <- if (is.list(m1_drug_pw) && !is.null(m1_drug_pw$emmeans)) m1_drug_pw$emmeans else m1_drug_pw

emm_sum <- as.data.frame(summary(emm_obj, type = "response", infer = TRUE))

lower_col <- intersect(c("lower.CL", "asymp.LCL", "lower.HPD"), names(emm_sum))[1]
upper_col <- intersect(c("upper.CL", "asymp.UCL", "upper.HPD"), names(emm_sum))[1]
if (is.na(lower_col) || is.na(upper_col)) {
  stop("Could not find CI columns in emmeans summary. Columns are: ",
       paste(names(emm_sum), collapse = ", "))
}

plot_dat <- emm_sum %>%
  transmute(
    Drug = factor(Drug, levels = c("placebo", "morphine", "naltrexone")),
    FixTimePerc = response * 100,
    lower = .data[[lower_col]] * 100,
    upper = .data[[upper_col]] * 100
  )

drug_cols <- c(
  morphine   = "#4ef59c",
  placebo    = "gray70",
  naltrexone = "#c21345"
)

y_lim <- ylim0(plot_dat$upper, extra_top = 0.10)

p_drug <- ggplot(plot_dat, aes(x = Drug, y = FixTimePerc, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +   # <- no space below 0
  coord_cartesian(ylim = y_lim) +
  labs(
    x = NULL,
    y = "Fixation Time (%)",
    title = "Estimated fixation time in eye region by Drug (Model 1, additional dataset)",
    subtitle = "Bars = emmeans on response scale; error bars = 95% CI"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_drug

out_dir <- here("results", "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ggsave(file.path(out_dir, "fixation_eye_emm_drug_model1_additional_slope.png"),
       p_drug, width = 7, height = 5, dpi = 300)

cat("Saved plots to:\n", out_dir, "\n")

# ============================================================
# MODEL 2: Drug by ASQ (low/high) plot
# ============================================================

m2_low  <- posthoc$Model2$drug_low_asq
m2_high <- posthoc$Model2$drug_high_asq

get_emm_part <- function(x) {
  if (is.list(x) && !is.null(x$emmeans)) x$emmeans else x
}

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

# annotation (high ASQ only)
y_annot <- plot_dat %>%
  filter(ASQ_level == "High ASQ (75th pct)") %>%
  summarise(y = max(upper, na.rm = TRUE)) %>%
  pull(y)

annot_df <- tibble(
  ASQ_level = factor("High ASQ (75th pct)", levels = levels(plot_dat$ASQ_level)),
  x1 = "morphine",
  x2 = "naltrexone",
  y  = y_annot + 0.03 * y_annot,
  label = "M vs N: OR = 2.006, p = .010"
)

# y-lims must include annotation height, but START AT 0
ymax_m2 <- max(plot_dat$upper, annot_df$y, na.rm = TRUE)
y_lim <- ylim0(ymax_m2, extra_top = 0.12)

p_h3 <- ggplot(plot_dat, aes(x = Drug, y = FixTimePerc, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  facet_wrap(~ ASQ_level) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = y_lim, clip = "off") +
  labs(
    x = NULL,
    y = "Fixation Time (%)",
    title = "Drug effects at low vs high ASQ (Model 2: Drug × ASQ)",
    subtitle = "Bars = emmeans on response scale; error bars = 95% CI"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 20, 10, 20)
  ) +
  geom_segment(
    data = annot_df,
    aes(x = x1, xend = x2, y = y, yend = y),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_segment(
    data = annot_df,
    aes(x = x1, xend = x1, y = y, yend = y - 0.01 * ymax_m2),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_segment(
    data = annot_df,
    aes(x = x2, xend = x2, y = y, yend = y - 0.01 * ymax_m2),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_text(
    data = annot_df,
    aes(x = "morphine", y = y + 0.02 * ymax_m2, label = label),
    inherit.aes = FALSE,
    hjust = 0.5,
    size = 3.5
  )

p_h3

ggsave(file.path(out_dir, "model2_drug_by_asq_low_high_barplot.png"),
       p_h3, width = 8, height = 5, dpi = 300)

cat("Saved plots to:\n", out_dir, "\n")

# ============================================================
# EH4.1 plot using MODEL 1 (Genetics main effect)
# ============================================================

models <- readRDS(here("models", "all_glmm_models_slope.rds"))
model1 <- models$model1

mf <- model1$frame
fac_vars <- c("Drug", "Genetics", "FaceGender", "Gaze2", "AttrLevel", "Imagelist", "Session", "ID")
for (v in fac_vars) {
  if (v %in% names(mf) && !is.factor(mf[[v]])) mf[[v]] <- factor(mf[[v]])
}
if ("Genetics" %in% names(mf)) mf$Genetics <- factor(mf$Genetics, levels = c("A", "G"))
if ("Drug" %in% names(mf))     mf$Drug     <- factor(mf$Drug, levels = c("placebo","morphine","naltrexone"))

nuis <- c("Drug", "FaceGender", "Gaze2", "AttrLevel", "Imagelist", "Session")
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

gen_cols <- c(A = "#F8766D", G = "#00BFC4")

y_lim <- ylim0(plot_dat$upper, extra_top = 0.12)

p_EH41 <- ggplot(plot_dat, aes(x = Genetics, y = FixTimePerc, fill = Genetics)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  scale_fill_manual(values = gen_cols, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = y_lim) +
  labs(
    x = "Genotype",
    y = "Fixation Time (%)",
    title = "EH4.1 (Model 1): Eye-region fixation by genotype",
    subtitle = "Bars = predicted means on response scale (ASQ at mean); error bars = 95% CI"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none")

p_EH41

ggsave(file.path(out_dir, "model1_genotype_fixation_EH4_1.png"), p_EH41, width = 7, height = 5, dpi = 300)

cat("Saved plots to:\n", out_dir, "\n")

# ============================================================
# Model 3: Barplot (Genotype difference at mean ASQ) ONLY
# (Your interaction plot is already fine; this fixes the barplot baseline)
# ============================================================

model3 <- models$model3
mf <- model3$frame
for (v in fac_vars) {
  if (v %in% names(mf) && !is.factor(mf[[v]])) mf[[v]] <- factor(mf[[v]])
}
if ("Drug" %in% names(mf))     mf$Drug     <- factor(mf$Drug, levels = c("placebo","morphine","naltrexone"))
if ("Genetics" %in% names(mf)) mf$Genetics <- factor(mf$Genetics, levels = c("A","G"))

nuis3 <- c("Drug", "FaceGender", "Gaze2", "AttrLevel", "Imagelist", "Session")
ASQ_mean <- mean(df_eye$ASQ, na.rm = TRUE)

rg_B <- ref_grid(
  model3,
  data = mf,
  at = list(ASQ = ASQ_mean),
  nuisance = nuis3,
  type = "response"
)

emm_gen_at_mean_asq <- emmeans(rg_B, ~ Genetics)
gen_df <- as.data.frame(summary(emm_gen_at_mean_asq, infer = TRUE, type = "response"))

lower_col <- intersect(c("lower.CL", "asymp.LCL", "lower.HPD"), names(gen_df))[1]
upper_col <- intersect(c("upper.CL", "asymp.UCL", "upper.HPD"), names(gen_df))[1]
if (is.na(lower_col) || is.na(upper_col)) {
  stop("Could not find CI columns. Available: ", paste(names(gen_df), collapse = ", "))
}

plot_bars <- gen_df %>%
  transmute(
    Genetics,
    FixTimePerc = response * 100,
    lower = .data[[lower_col]] * 100,
    upper = .data[[upper_col]] * 100
  )

# extra top space for the annotation text
y_lim <- ylim0(max(plot_bars$upper, na.rm = TRUE), extra_top = 0.30)

p_B <- ggplot(plot_bars, aes(x = Genetics, y = FixTimePerc, fill = Genetics)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  scale_fill_manual(values = gen_cols, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = y_lim, clip = "off") +
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
    y = y_lim[2] * 0.95,
    label = "Genotype effect: χ²(1) = 5.26, p = .022\nOpposite direction: b = −0.731, SE = 0.319, z = −2.29, p = .022\nTukey OR(A/G) = 2.08, p = .022",
    size = 3.5
  )

p_B

ggsave(file.path(out_dir, "model3_genotype_difference_at_mean_asq.png"), p_B, width = 8, height = 5, dpi = 300)

cat("Saved plots to:\n", out_dir, "\n")

# ============================================================
# Model 5 simple-effects plot (Drug within Genetics @ mean ASQ)
# FIXED: y-axis starts at 0% (no bars sinking into x labels)
# ============================================================

library(tidyverse)
library(emmeans)
library(here)
library(readr)

# helper: y-limits starting at 0 with headroom
ylim0 <- function(ymax, extra_top = 0.12) c(0, ymax + extra_top * ymax)

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

# -----------------------
# Colors
# -----------------------
drug_cols <- c(
  morphine   = "#4ef59c",
  placebo    = "gray70",
  naltrexone = "#c21345"
)

# -----------------------
# Annotation ONLY in Genetics = G facet
# -----------------------
yG <- plot_dat %>%
  filter(Genetics == "G") %>%
  summarise(y = max(upper, na.rm = TRUE)) %>%
  pull(y)

annot_df <- tibble(
  Genetics = factor("G", levels = levels(plot_dat$Genetics)),
  x1 = "placebo",
  x2 = "morphine",
  y  = yG * 1.06,  # slightly above the highest CI in G
  label = "placebo vs morphine: OR = 0.569, p = .017"
)

# -----------------------
# FIXED y-limits: start at 0 and include annotation height
# -----------------------
ymax_all <- max(plot_dat$upper, annot_df$y, na.rm = TRUE)
y_lim <- ylim0(ymax_all, extra_top = 0.12)

# -----------------------
# Plot
# -----------------------
p_simple <- ggplot(plot_dat, aes(x = Drug, y = FixTimePerc, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  facet_wrap(~ Genetics) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  # <- no space below 0
  coord_cartesian(ylim = y_lim, clip = "off") +
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
    aes(x = x1, xend = x1, y = y, yend = y - 0.015 * ymax_all),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_segment(
    data = annot_df,
    aes(x = x2, xend = x2, y = y, yend = y - 0.015 * ymax_all),
    inherit.aes = FALSE, linewidth = 0.7
  ) +
  geom_text(
    data = annot_df,
    aes(x = "morphine", y = y + 0.03 * ymax_all, label = label),
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
