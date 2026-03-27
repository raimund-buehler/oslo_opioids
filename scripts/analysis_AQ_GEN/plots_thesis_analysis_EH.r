#All plots from the exploratory analyses as preregistered

# ============================================================
# PLOT: EH4.1 (Model 1): fixation by genotype + OR/effect bracket
# ============================================================
plot_eh41_full <- df_all_eye %>%
  filter(!is.na(Genetics)) %>%
  group_by(Genetics) %>%
  summarise(
    n = sum(!is.na(FixTimePerc)),
    mean = mean(FixTimePerc, na.rm = TRUE),
    sd   = sd(FixTimePerc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Genetics = factor(Genetics, levels = c("A","G")),
    lower = mean - sd,
    upper = mean + sd
  )

ASQ_mean_full <- mean(df_all_eye$ASQ, na.rm = TRUE)

nuis1b <- intersect(c("Drug","FaceGender","Gaze2","AttrLevel","Imagelist","Session"), names(mf1))

rg1_gen <- ref_grid(
  model1, data = mf1,
  at = list(ASQ = ASQ_mean_full, AOI = "eye_brow"),
  nuisance = nuis1b,
  type = "response"
)

emm_gen1 <- emmeans(rg1_gen, ~ Genetics)
gen_lvls <- levels(emm_gen1@grid$Genetics)

ctr_gen1 <- contrast(emm_gen1, method = list("A vs G" = make_ab_vec(gen_lvls, "A", "G")))
or1 <- extract_or_p(ctr_gen1)

lab_eh41 <- label_effect_p("A vs G", or1$label, or1$est, or1$p)

ymax_eh <- max(plot_eh41_full$upper, na.rm = TRUE)
y_lim_eh <- ylim0(ymax_eh, extra_top = 0.40)

pval_eh <- tibble(
  group1 = "A", group2 = "G",
  y.position = ymax_eh + diff(y_lim_eh) * 0.12,
  label = lab_eh41
)

p_EH41 <- ggplot(plot_eh41_full, aes(x = Genetics, y = mean, fill = Genetics)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  scale_fill_manual(values = gen_cols, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = y_lim_eh, clip = "off") +
  ggpubr::stat_pvalue_manual(
    pval_eh,
    label = "label",
    xmin = "group1", xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    bracket.size = 0.6,
    size = 3.6
  ) +
  labs(
    x = "Genotype",
    y = "Fixation Time (%)",
    title = "Eye-region fixation by genotype"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.margin = margin(10, 20, 10, 20))

# ============================================================
# PLOT: EH4.2 (Model 3): ASQ × Genotype interaction + density
# ============================================================
mf3 <- model3$frame
for (v in fac_vars) if (v %in% names(mf3) && !is.factor(mf3[[v]])) mf3[[v]] <- factor(mf3[[v]])
if ("Drug" %in% names(mf3)) mf3$Drug <- factor(mf3$Drug, levels = drug_levels)
if ("Genetics" %in% names(mf3)) mf3$Genetics <- factor(mf3$Genetics, levels = c("A","G"))

asq_seq3 <- seq(
  quantile(df_eye$ASQ, 0.02, na.rm = TRUE),
  quantile(df_eye$ASQ, 0.98, na.rm = TRUE),
  length.out = 120
)

nuis3 <- intersect(c("Drug","FaceGender","Gaze2","AttrLevel","Imagelist","Session"), names(mf3))

rg3 <- ref_grid(
  model3,
  data = mf3,
  at = list(ASQ = asq_seq3, AOI = "eye_brow", Genetics = c("A","G")),
  nuisance = nuis3,
  type = "response"
)

emm_asq_gen <- emmeans(rg3, ~ ASQ | Genetics)
pred_df3 <- as.data.frame(summary(emm_asq_gen, infer = TRUE, type = "response"))

lower_col3 <- intersect(c("lower.CL", "asymp.LCL", "lower.HPD"), names(pred_df3))[1]
upper_col3 <- intersect(c("upper.CL", "asymp.UCL", "upper.HPD"), names(pred_df3))[1]
if (is.na(lower_col3) || is.na(upper_col3)) {
  stop("Could not find CI columns in Model3 emmeans output. Columns: ", paste(names(pred_df3), collapse = ", "))
}

pred_df3 <- pred_df3 %>%
  transmute(
    Genetics = factor(Genetics, levels = c("A","G")),
    ASQ = ASQ,
    fit   = response * 100,
    lower = .data[[lower_col3]] * 100,
    upper = .data[[upper_col3]] * 100
  )

y_top3 <- max(pred_df3$upper, na.rm = TRUE)
density_height3 <- 0.18 * y_top3

p_EH42 <- ggplot() +
  geom_density(
    data = df_eye,
    aes(x = ASQ, y = after_stat(scaled) * density_height3),
    inherit.aes = FALSE,
    fill = "grey60",
    alpha = 0.30,
    color = NA
  ) +
  geom_ribbon(
    data = pred_df3,
    aes(x = ASQ, ymin = lower, ymax = upper, fill = Genetics),
    alpha = 0.20
  ) +
  geom_line(
    data = pred_df3,
    aes(x = ASQ, y = fit, color = Genetics),
    linewidth = 1
  ) +
  scale_color_manual(values = gen_cols, name = "Genotype") +
  scale_fill_manual(values = gen_cols, name = "Genotype") +
  scale_y_continuous(limits = c(0, y_top3 * 1.05), expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "ASQ",
    y = "Fixation Time (%)",
    title = "Predicted fixation across ASQ by genotype"
  ) +
  theme_minimal(base_size = 12)

ggsave(file.path(out_dir, "model1_genotype_fixation_EH4_1.png"),
       p_EH41, width = 7.5, height = 5.5, dpi = 300)

ggsave(file.path(out_dir, "EH4_2_ASQ_by_Genotype_interaction_model3.png"),
       p_EH42, width = 8.5, height = 5.5, dpi = 300)

# EH5
# ============================================================
# EH5.1 / EH5.2 (Model 4) + Simple effects bar plot (Model 4) + EH5.3 (three-way plot with MEDIAN split for ASQ)
# ============================================================

library(tidyverse)
library(here)
library(readr)
library(emmeans)
library(ggpubr)

# -----------------------
# Global settings
# -----------------------
drug_levels <- c("morphine", "placebo", "naltrexone")
geno_levels <- c("A", "G")

gen_cols  <- c(A = "#F8766D", G = "#00BFC4")
drug_cols <- c(morphine = "#4ef59c", placebo = "gray70", naltrexone = "#c21345")

out_dir <- here("results", "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# Helpers
# -----------------------
ylim0 <- function(ymax_or_vec, extra_top = 0.15) {
  ymax <- if (length(ymax_or_vec) > 1) max(ymax_or_vec, na.rm = TRUE) else ymax_or_vec
  c(0, ymax + extra_top * ymax)
}

# Vectorized: stars if significant; else "p = .34"
fmt_sig <- function(p) {
  p <- as.numeric(p)
  out <- rep(NA_character_, length(p))

  idx_ok <- !is.na(p)
  out[idx_ok] <- paste0("p = ", sub("^0", "", sprintf("%.2f", p[idx_ok])))

  out[idx_ok & p < .05]  <- "*"
  out[idx_ok & p < .01]  <- "**"
  out[idx_ok & p < .001] <- "***"

  out[is.na(p)] <- "p = NA"
  out
}

# append as " ***" if sig, else ", p = .34"
fmt_sig_suffix <- function(p) {
  s <- fmt_sig(p)
  ifelse(s %in% c("*","**","***"), paste0(" ", s), paste0(", ", s))
}

fmt_est2 <- function(x) sprintf("%.2f", as.numeric(x))

get_ci_cols <- function(df) {
  lower_col <- intersect(c("lower.CL","asymp.LCL","lower.HPD"), names(df))[1]
  upper_col <- intersect(c("upper.CL","asymp.UCL","upper.HPD"), names(df))[1]
  if (is.na(lower_col) || is.na(upper_col)) {
    stop("Could not find CI cols. Available: ", paste(names(df), collapse = ", "))
  }
  list(lower = lower_col, upper = upper_col)
}

get_pred_col <- function(df) {
  if ("response" %in% names(df)) return("response")
  if ("emmean" %in% names(df))   return("emmean")
  stop("Could not find prediction column (response/emmean). Columns: ", paste(names(df), collapse = ", "))
}

make_ab_vec <- function(levels_vec, a, b) {
  if (!a %in% levels_vec || !b %in% levels_vec) {
    stop("Requested contrast levels not found. Have: ",
         paste(levels_vec, collapse = ", "),
         " | requested: ", a, " vs ", b)
  }
  v <- rep(0, length(levels_vec))
  v[match(a, levels_vec)] <-  1
  v[match(b, levels_vec)] <- -1
  v
}

extract_or_p <- function(contr_obj) {
  s <- as.data.frame(summary(contr_obj, infer = TRUE, type = "response"))
  est_col <- intersect(c("odds.ratio","ratio","response","estimate"), names(s))[1]
  if (is.na(est_col)) stop("No estimate column in contrast summary. Columns: ", paste(names(s), collapse = ", "))
  list(
    est = as.numeric(s[[est_col]][1]),
    p   = as.numeric(s$p.value[1]),
    label = if (est_col %in% c("odds.ratio","ratio")) "OR" else "effect"
  )
}

# ============================================================
# Load data (FULL dataset, eye region) + set factor order
# ============================================================
df <- read_csv(here("data", "analyses", "fix_perc_ASQ_GEN.csv"), show_col_types = FALSE)

df_all_eye <- df %>%
  filter(AOI == "eye_brow") %>%
  mutate(
    ID   = factor(as.character(ID)),
    Drug = factor(as.character(Drug), levels = drug_levels)
  )

if (!"FixTimePerc" %in% names(df_all_eye)) stop("FixTimePerc column not found in data.")
ASQ_mean_full <- mean(df_all_eye$ASQ, na.rm = TRUE)

if ("Genetics" %in% names(df_all_eye)) {
  df_all_eye <- df_all_eye %>% mutate(Genetics = factor(as.character(Genetics), levels = geno_levels))
}
if ("Genotype" %in% names(df_all_eye)) {
  df_all_eye <- df_all_eye %>% mutate(Genotype = factor(as.character(Genotype), levels = geno_levels))
}

# Unified genotype column for raw plots
if ("Genetics" %in% names(df_all_eye)) {
  df_all_eye$GenotypePlot <- df_all_eye$Genetics
} else if ("Genotype" %in% names(df_all_eye)) {
  df_all_eye$GenotypePlot <- df_all_eye$Genotype
} else {
  stop("Neither 'Genetics' nor 'Genotype' found in the CSV.")
}
df_all_eye$GenotypePlot <- factor(as.character(df_all_eye$GenotypePlot), levels = geno_levels)

# ============================================================
# Load models (Model 4 + Model 5)
# ============================================================
models <- readRDS(here("models", "all_glmm_models_slope.rds"))
if (!"model4" %in% names(models)) stop("models$model4 not found in all_glmm_models_slope.rds")
if (!"model5" %in% names(models)) stop("models$model5 not found in all_glmm_models_slope.rds")

model4 <- models$model4
model5 <- models$model5

# ============================================================
# MODEL 4 SETUP
# ============================================================
mf4 <- model4$frame

geno_var4 <- if ("Genetics" %in% names(mf4)) {
  "Genetics"
} else if ("Genotype" %in% names(mf4)) {
  "Genotype"
} else {
  stop(
    "Model 4 frame has neither 'Genetics' nor 'Genotype'. Names: ",
    paste(names(mf4), collapse = ", ")
  )
}

fac_vars4 <- c(
  "Drug", geno_var4, "FaceGender", "Gaze2", "AttrLevel",
  "Imagelist", "Session", "AOI", "ID", "Participant"
)

for (v in fac_vars4) {
  if (v %in% names(mf4) && !is.factor(mf4[[v]])) {
    mf4[[v]] <- factor(mf4[[v]])
  }
}

if ("Drug" %in% names(mf4)) {
  mf4$Drug <- factor(as.character(mf4$Drug), levels = drug_levels)
}

if (geno_var4 %in% names(mf4)) {
  mf4[[geno_var4]] <- factor(as.character(mf4[[geno_var4]]), levels = geno_levels)
}

# Base 'at' list for predictions
# Important: do NOT lock Drug here if you want separate morphine/naltrexone estimates later
at_base4 <- list(
  ASQ = ASQ_mean_full
)

if ("AOI" %in% names(mf4)) {
  at_base4$AOI <- "eye_brow"
}

if (geno_var4 %in% names(mf4)) {
  at_base4[[geno_var4]] <- geno_levels
}

# Optional: create ref_grid if you need it elsewhere
rg4 <- ref_grid(
  model4,
  data = mf4,
  at = c(at_base4, list(Drug = drug_levels)),
  type = "response"
)

# ============================================================
# HELPER FUNCTION FOR GENOTYPE-EFFECT PLOTS WITHIN A DRUG
# ============================================================
get_genotype_plot_under_drug <- function(drug_name, plot_title) {
  
  emm <- emmeans(
    model4,
    specs = as.formula(paste0("~ ", geno_var4)),
    data  = mf4,
    at    = c(at_base4, list(Drug = drug_name)),
    type  = "response"
  )
  
  emm_df <- as.data.frame(summary(emm, infer = TRUE, type = "response"))
  
  ci_cols  <- get_ci_cols(emm_df)
  pred_col <- get_pred_col(emm_df)
  
  mult <- if (max(emm_df[[pred_col]], na.rm = TRUE) <= 1.5) 100 else 1
  
  plot_df <- emm_df %>%
    transmute(
      Genotype = factor(as.character(.data[[geno_var4]]), levels = geno_levels),
      fit   = .data[[pred_col]] * mult,
      lower = .data[[ci_cols$lower]] * mult,
      upper = .data[[ci_cols$upper]] * mult
    )
  
  # Use actual factor levels from the model frame
  geno_lvls <- levels(mf4[[geno_var4]])
  
  ctr <- contrast(
    emm,
    method = list("A vs G" = make_ab_vec(geno_lvls, "A", "G"))
  )
  
  or_out <- extract_or_p(ctr)
  
  lab <- paste0(
    "A vs G: ", or_out$label, " = ", fmt_est2(or_out$est),
    fmt_sig_suffix(or_out$p)
  )
  
  ymax  <- max(plot_df$upper, na.rm = TRUE)
  y_lim <- ylim0(ymax, extra_top = 0.35)
  
  pval_df <- tibble(
    group1 = "A",
    group2 = "G",
    y.position = ymax + diff(y_lim) * 0.12,
    label = lab
  )
  
  p <- ggplot(plot_df, aes(x = Genotype, y = fit, fill = Genotype)) +
    geom_col(width = 0.7) +
    geom_errorbar(
      aes(ymin = lower, ymax = upper),
      width = 0.15,
      linewidth = 0.6
    ) +
    ggpubr::stat_pvalue_manual(
      pval_df,
      label = "label",
      xmin = "group1",
      xmax = "group2",
      y.position = "y.position",
      tip.length = 0.01,
      bracket.size = 0.6,
      size = 3.6
    ) +
    scale_fill_manual(values = gen_cols, drop = FALSE) +
    coord_cartesian(ylim = y_lim, clip = "off") +
    labs(
      x = NULL,
      y = "Predicted fixation time in eye region (%)",
      title = plot_title
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      plot.margin = margin(10, 25, 10, 25)
    )
  
  list(
    emm      = emm,
    emm_df   = emm_df,
    contrast = ctr,
    plot_df  = plot_df,
    plot     = p
  )
}

# ============================================================
# EH5.1: Under morphine – genotype effect (A vs G) [Model 4]
# ============================================================
res_morph <- get_genotype_plot_under_drug(
  drug_name  = "morphine",
  plot_title = "Genotype effect under morphine (ASQ at mean)"
)

p_EH5_1 <- res_morph$plot

# ============================================================
# EH5.2: Under naltrexone – genotype effect (A vs G) [Model 4]
# ============================================================
res_nalt <- get_genotype_plot_under_drug(
  drug_name  = "naltrexone",
  plot_title = "Genotype effect under naltrexone (ASQ at mean)"
)

p_EH5_2 <- res_nalt$plot

# ============================================================
# OPTIONAL CHECKS
# ============================================================
summary(res_morph$emm, infer = TRUE, type = "response")
summary(res_nalt$emm,  infer = TRUE, type = "response")

summary(res_morph$contrast, infer = TRUE, type = "response")
summary(res_nalt$contrast,  infer = TRUE, type = "response")



# ============================================================
# Simple effects BAR plot (Model 4): Drug differences within genotype
# FIXED: contrast direction = morphine vs placebo
# Output: model4_simple_effects.png
# ============================================================
plot_m4_full <- df_all_eye %>%
  filter(!is.na(GenotypePlot), !is.na(Drug)) %>%
  mutate(
    Drug = factor(as.character(Drug), levels = drug_levels),
    GenotypePlot = factor(as.character(GenotypePlot), levels = geno_levels)
  ) %>%
  group_by(GenotypePlot, Drug) %>%
  summarise(
    n = sum(!is.na(FixTimePerc)),
    mean = mean(FixTimePerc, na.rm = TRUE),
    sd   = sd(FixTimePerc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Drug = factor(as.character(Drug), levels = drug_levels),
    GenotypePlot = factor(as.character(GenotypePlot), levels = geno_levels),
    lower = mean - sd,
    upper = mean + sd
  )

emm_drug_by_gen <- emmeans(
  rg4,
  specs = as.formula(paste0("~ Drug | ", geno_var4)),
  at = list(Drug = drug_levels)
)

drug_lvls <- levels(emm_drug_by_gen@grid$Drug)

ctr_mp <- contrast(
  emm_drug_by_gen,
  method = list("morphine vs placebo" = make_ab_vec(drug_lvls, "morphine", "placebo")),
  by = geno_var4
)

s_mp <- as.data.frame(summary(ctr_mp, infer = TRUE, type = "response"))
rowG <- s_mp %>% filter(.data[[geno_var4]] == "G") %>% slice(1)
if (nrow(rowG) == 0) stop("Could not find Genotype == 'G' in Model 4 contrast output.")

est_col <- intersect(c("odds.ratio","ratio","response","estimate"), names(rowG))[1]
labtype <- if (est_col %in% c("odds.ratio","ratio")) "OR" else "effect"

lab_m4 <- paste0(
  "morphine vs placebo: ", labtype, " = ", fmt_est2(rowG[[est_col]]),
  fmt_sig_suffix(as.numeric(rowG$p.value))
)

yG <- plot_m4_full %>%
  filter(GenotypePlot == "G") %>%
  summarise(y = max(upper, na.rm = TRUE)) %>%
  pull(y)

ymax_m4 <- max(plot_m4_full$upper, na.rm = TRUE)
y_lim_m4 <- ylim0(max(ymax_m4, yG * 1.25), extra_top = 0.25)

pval_m4 <- tibble(
  GenotypePlot = factor("G", levels = geno_levels),
  group1 = "morphine",
  group2 = "placebo",
  y.position = yG + diff(y_lim_m4) * 0.10,
  label = lab_m4
)

p_m4_simple <- ggplot(plot_m4_full, aes(x = Drug, y = mean, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  facet_wrap(~ GenotypePlot) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  scale_x_discrete(limits = drug_levels, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = y_lim_m4, clip = "off") +
  ggpubr::stat_pvalue_manual(
    pval_m4,
    label = "label",
    xmin = "group1", xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    bracket.size = 0.6,
    size = 3.6
  ) +
  labs(
    x = NULL,
    y = "Fixation Time (%)",
    title = "Simple effects: Drug differences within genotype"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 20, 10, 20)
  )

# ============================================================
# NEW PLOT (Model 4): same style as model4_simple_effects,
# but 3 brackets = A vs G within each drug
# Saves: EH5_1_Genotype_effects_under_drugs_model4.png
# ============================================================
plot_for_new <- plot_m4_full %>%
  mutate(
    Drug = factor(as.character(Drug), levels = drug_levels),
    GenotypePlot = factor(as.character(GenotypePlot), levels = geno_levels)
  ) %>%
  arrange(Drug, GenotypePlot)

emm_gen_by_drug <- emmeans(
  rg4,
  specs = as.formula(paste0("~ ", geno_var4, " | Drug")),
  at = list(Drug = drug_levels)
)

geno_lvls <- levels(emm_gen_by_drug@grid[[geno_var4]])

ctr_avg_by_drug <- contrast(
  emm_gen_by_drug,
  method = list("A vs G" = make_ab_vec(geno_lvls, "A", "G")),
  by = "Drug"
)

s_avg <- as.data.frame(summary(ctr_avg_by_drug, infer = TRUE, type = "response"))

est_col2 <- intersect(c("odds.ratio","ratio","response","estimate"), names(s_avg))[1]
if (is.na(est_col2)) stop("No estimate column found in s_avg.")
labtype2 <- if (est_col2 %in% c("odds.ratio","ratio")) "OR" else "effect"

bar_w   <- 0.55
dodge_w <- 0.90
offset  <- dodge_w / 4
pos_d   <- position_dodge(width = dodge_w)

ymax_new   <- max(plot_for_new$upper, na.rm = TRUE)
y_lim_new  <- ylim0(ymax_new, extra_top = 0.25)
y_lim_new2 <- y_lim_new
y_lim_new2[1] <- -diff(y_lim_new) * 0.10

y_by_drug <- plot_for_new %>%
  group_by(Drug) %>%
  summarise(y = max(upper, na.rm = TRUE), .groups = "drop")

drug_num <- tibble(
  Drug = factor(drug_levels, levels = drug_levels),
  x = seq_along(drug_levels)
)

pval_geno_drug <- s_avg %>%
  transmute(
    Drug = factor(as.character(Drug), levels = drug_levels),
    group1 = "A",
    group2 = "G",
    label = paste0("A vs G: ", labtype2, " = ", fmt_est2(.data[[est_col2]]), fmt_sig_suffix(p.value))
  ) %>%
  left_join(y_by_drug, by = "Drug") %>%
  left_join(drug_num, by = "Drug") %>%
  mutate(
    xmin = x - offset,
    xmax = x + offset,
    y.position = y + diff(y_lim_new2) * 0.10
  ) %>%
  select(group1, group2, xmin, xmax, y.position, label)

geno_labels <- plot_for_new %>%
  distinct(Drug, GenotypePlot) %>%
  mutate(
    y = y_lim_new2[1] + diff(y_lim_new2) * 0.02,
    label = as.character(GenotypePlot)
  )

p_EH5_1_all_drugs <- ggplot(
  plot_for_new,
  aes(x = Drug, y = mean, fill = Drug, group = GenotypePlot)
) +
  geom_col(width = bar_w, position = pos_d) +
  geom_errorbar(
    aes(ymin = lower, ymax = upper),
    width = 0.15,
    linewidth = 0.6,
    position = pos_d
  ) +
  geom_text(
    data = geno_labels,
    aes(x = Drug, y = y, label = label, group = GenotypePlot),
    position = pos_d,
    inherit.aes = FALSE,
    size = 3.8
  ) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  scale_x_discrete(limits = drug_levels, drop = FALSE) +
  coord_cartesian(ylim = y_lim_new2, clip = "off") +
  ggpubr::stat_pvalue_manual(
    pval_geno_drug,
    label = "label",
    xmin = "xmin", xmax = "xmax",
    y.position = "y.position",
    tip.length = 0.01,
    bracket.size = 0.6,
    size = 3.6
  ) +
  labs(
    x = NULL,
    y = "Fixation Time (%)",
    title = "Genotype effects under morphine, placebo or naltrexone"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 20, 28, 20)
  )

# -----------------------
# Save Model 4 plots
# -----------------------
ggsave(
  file.path(out_dir, "EH5_1_morphine_genotype_effect_model4.png"),
  p_EH5_1, width = 7.5, height = 5.5, dpi = 300
)

ggsave(
  file.path(out_dir, "EH5_2_naltrexone_genotype_effect_model4.png"),
  p_EH5_2, width = 7.5, height = 5.5, dpi = 300
)

ggsave(
  file.path(out_dir, "model4_simple_effects.png"),
  p_m4_simple, width = 9, height = 5.5, dpi = 300
)

ggsave(
  file.path(out_dir, "EH5_1_Genotype_effects_under_drugs_model4.png"),
  p_EH5_1_all_drugs, width = 9, height = 5.5, dpi = 300
)

cat("Saved Model 4 plots to:\n", out_dir, "\n")

# ============================================================
# MODEL 5: THREE-WAY interaction plot (Drug × Genotype × ASQ)
# MEDIAN split applies here
# ROBUST FIX:
# - use the model's native Drug level order for prediction mapping
# - then reorder for display as morphine - placebo - naltrexone
# Saves: Model5_Drug_by_Genotype_by_ASQ_low_high.png
# ============================================================

mf5 <- model5$frame

geno_var5 <- if ("Genetics" %in% names(mf5)) "Genetics" else if ("Genotype" %in% names(mf5)) "Genotype" else {
  stop("Model 5 frame has neither 'Genetics' nor 'Genotype'. Names: ", paste(names(mf5), collapse = ", "))
}

# Keep desired DISPLAY order
drug_levels <- c("morphine", "placebo", "naltrexone")
geno_levels <- c("A", "G")

fac_vars5 <- c("Drug", geno_var5, "FaceGender","Gaze2","AttrLevel","Imagelist","Session","AOI","ID","Participant")
for (v in fac_vars5) {
  if (v %in% names(mf5) && !is.factor(mf5[[v]])) mf5[[v]] <- factor(mf5[[v]])
}

# For prediction data we still set factors cleanly,
# but for remapping we will use the model's ORIGINAL/native Drug order
if ("Drug" %in% names(mf5)) {
  mf5$Drug <- factor(as.character(mf5$Drug))
}
if (geno_var5 %in% names(mf5)) {
  mf5[[geno_var5]] <- factor(as.character(mf5[[geno_var5]]), levels = geno_levels)
}

# -----------------------
# Get native Drug order from the fitted model
# -----------------------
fit_drug_levels <- NULL

if (!is.null(model5$xlevels) && "Drug" %in% names(model5$xlevels)) {
  fit_drug_levels <- as.character(model5$xlevels$Drug)
}

if (is.null(fit_drug_levels)) {
  mf5_native <- tryCatch(model.frame(model5), error = function(e) NULL)
  if (!is.null(mf5_native) && "Drug" %in% names(mf5_native)) {
    fit_drug_levels <- levels(mf5_native$Drug)
  }
}

if (is.null(fit_drug_levels)) {
  stop("Could not recover the native Drug level order from model5.")
}

if (!setequal(fit_drug_levels, drug_levels)) {
  stop(
    "Native Drug levels in model5 do not match expected set.\n",
    "Model levels: ", paste(fit_drug_levels, collapse = ", "), "\n",
    "Expected: ", paste(drug_levels, collapse = ", ")
  )
}

cat("Model 5 native Drug order used for remapping: ",
    paste(fit_drug_levels, collapse = " -> "), "\n", sep = "")

# -----------------------
# Median split on full eye_brow dataset
# -----------------------
asq_med  <- median(df_all_eye$ASQ, na.rm = TRUE)
asq_low  <- median(df_all_eye$ASQ[df_all_eye$ASQ <= asq_med], na.rm = TRUE)
asq_high <- median(df_all_eye$ASQ[df_all_eye$ASQ >  asq_med], na.rm = TRUE)

# IMPORTANT:
# do NOT force Drug order here in "at"
# because that can relabel rows without fixing the underlying mapping
at_list5 <- list(ASQ = c(asq_low, asq_high))
if ("AOI" %in% names(mf5)) at_list5$AOI <- "eye_brow"

rg5 <- ref_grid(model5, data = mf5, at = at_list5, type = "response")

emm5 <- emmeans(
  rg5,
  specs = as.formula(paste0("~ Drug * ", geno_var5, " | ASQ"))
)

emm5_df <- as.data.frame(summary(emm5, infer = TRUE, type = "response"))

resp_col <- intersect(c("response", "prob", "rate", "emmean"), names(emm5_df))[1]
if (is.na(resp_col)) {
  stop("Couldn't find response-scale column in emmeans output. Columns: ", paste(names(emm5_df), collapse = ", "))
}

lower_col <- intersect(c("lower.CL", "asymp.LCL", "LCL", "lower.HPD"), names(emm5_df))[1]
upper_col <- intersect(c("upper.CL", "asymp.UCL", "UCL", "upper.HPD"), names(emm5_df))[1]
if (is.na(lower_col) || is.na(upper_col)) {
  stop("Couldn't find CI columns in emmeans output. Columns: ", paste(names(emm5_df), collapse = ", "))
}

mult5 <- if (max(emm5_df[[resp_col]], na.rm = TRUE) <= 1.5) 100 else 1

# -----------------------
# CRITICAL FIX:
# Re-assign Drug labels by ROW POSITION within each ASQ × Genotype block
# using the model's native Drug order.
# Then reorder for DISPLAY as morphine -> placebo -> naltrexone.
# -----------------------
plot_df5 <- emm5_df %>%
  group_by(ASQ, .data[[geno_var5]]) %>%
  mutate(
    .row_in_block = row_number(),
    Drug_native = fit_drug_levels[.row_in_block]
  ) %>%
  ungroup() %>%
  transmute(
    ASQ = ASQ,
    ASQ_level = factor(
      ifelse(ASQ <= asq_med, "Low ASQ (≤ median)", "High ASQ (> median)"),
      levels = c("Low ASQ (≤ median)", "High ASQ (> median)")
    ),
    Drug = factor(Drug_native, levels = drug_levels),
    Genetics = factor(as.character(.data[[geno_var5]]), levels = geno_levels),
    y    = .data[[resp_col]] * mult5,
    ymin = .data[[lower_col]] * mult5,
    ymax = .data[[upper_col]] * mult5
  ) %>%
  arrange(ASQ_level, Genetics, Drug)

# Optional console check
cat("\nRemapped plot_df5 preview:\n")
print(
  plot_df5 %>%
    select(ASQ_level, Genetics, Drug, y) %>%
    arrange(ASQ_level, Genetics, Drug)
)

p5 <- ggplot(plot_df5, aes(x = Drug, y = y, group = Genetics, color = Genetics)) +
  geom_point(position = position_dodge(width = 0.25), size = 2) +
  geom_line(position = position_dodge(width = 0.25), linewidth = 1) +
  geom_errorbar(
    aes(ymin = ymin, ymax = ymax),
    width = 0.12,
    position = position_dodge(width = 0.25),
    linewidth = 0.7
  ) +
  facet_wrap(~ ASQ_level) +
  scale_x_discrete(limits = drug_levels, drop = FALSE) +
  scale_color_manual(values = gen_cols, drop = FALSE) +
  labs(
    x = "Drug condition",
    y = if (mult5 == 100) "Predicted fixation in eye region (%)" else "Predicted fixation (eye region)",
    title = "Drug x Genotype x ASQ (ASQ was median-split)"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(out_dir, "Model5_Drug_by_Genotype_by_ASQ_low_high.png"),
  plot = p5, width = 9, height = 5.5, dpi = 300
)

cat("Saved Model 5 plot to:\n", out_dir, "\n")