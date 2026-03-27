# ============================================================
# ROBUST FINAL SCRIPT (CLEANED + GLOBAL FIXES)
# Fixes applied to ALL plots:
#  1) Drug factor order: morphine -> placebo -> naltrexone
#  2) All effects + p rounded to 2 decimals
#  3) Label rule everywhere:
#       - significant p:  "t = 3.41 **"   (stars only)
#       - non-significant: "t = 2.34, p = .54"  (comma + p, no leading 0)
#  4) Removed all plot subtitles
#  5) Median split (instead of 25/75 quantiles) for ASQ-splitting plot
# ============================================================

# -----------------------
# Packages
# -----------------------
pkgs <- c("tidyverse", "here", "readr", "emmeans", "ggpubr")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# -----------------------
# Global settings
# -----------------------
drug_levels <- c("morphine", "placebo", "naltrexone")

drug_cols <- c(morphine = "#4ef59c", placebo = "gray70", naltrexone = "#c21345")
gen_cols  <- c(A = "#F8766D", G = "#00BFC4")

out_dir <- here("results", "plots")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -----------------------
# Helpers
# -----------------------
ylim0 <- function(ymax_or_vec, extra_top = 0.15) {
  ymax <- if (length(ymax_or_vec) > 1) max(ymax_or_vec, na.rm = TRUE) else ymax_or_vec
  c(0, ymax + extra_top * ymax)
}

fmt2 <- function(x) sprintf("%.2f", x)

# p shown as ".54" instead of "0.54"
fmt_p2 <- function(p) sub("^0", "", sprintf("%.2f", p))

# vectorized suffix:
#  - significant -> " *" / " **" / " ***"
#  - non-significant -> ", p = .54"
#  - NA -> ", p = NA"
p_sig_suffix <- function(p) {
  out <- rep(NA_character_, length(p))
  out[is.na(p)] <- ", p = NA"
  out[!is.na(p) & p <  .001] <- " ***"
  out[!is.na(p) & p >= .001 & p < .01] <- " **"
  out[!is.na(p) & p >= .01  & p < .05] <- " *"
  idx <- !is.na(p) & p >= .05
  out[idx] <- paste0(", p = ", fmt_p2(p[idx]))
  out
}

get_emm_part <- function(x) if (is.list(x) && !is.null(x$emmeans)) x$emmeans else x

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
  est_col <- intersect(c("odds.ratio", "ratio", "response", "estimate"), names(s))[1]
  if (is.na(est_col)) stop("No estimate column found in contrast summary. Columns: ", paste(names(s), collapse = ", "))
  list(
    est = as.numeric(s[[est_col]][1]),
    p   = as.numeric(s$p.value[1]),
    label = if (est_col %in% c("odds.ratio","ratio")) "OR" else "effect"
  )
}

# Effect/OR label using the global suffix rule
label_effect_p <- function(prefix, eff_label, eff_value, p) {
  paste0(prefix, ": ", eff_label, " = ", fmt2(eff_value), p_sig_suffix(p))
}

# -----------------------
# Load DATA (FULL + additional)
# -----------------------
df <- read_csv(here("data", "analyses", "fix_perc_ASQ_GEN.csv"))

df_all_eye <- df %>%
  filter(AOI == "eye_brow") %>%
  mutate(
    ID = factor(as.character(ID)),
    Drug = factor(Drug, levels = drug_levels),
    Genetics = if ("Genetics" %in% names(.)) factor(Genetics, levels = c("A","G")) else Genetics
  )

df_add_eye <- df %>%
  mutate(ID_chr = as.character(ID)) %>%
  filter(grepl("^2", ID_chr)) %>%
  filter(AOI == "eye_brow") %>%
  mutate(
    ID = factor(ID_chr),
    Drug = factor(Drug, levels = drug_levels),
    Genetics = if ("Genetics" %in% names(.)) factor(Genetics, levels = c("A","G")) else Genetics
  ) %>%
  select(-ID_chr)

cat("N FULL eye:", nrow(df_all_eye), "| subjects:", n_distinct(df_all_eye$ID), "\n")
cat("N ADD  eye:", nrow(df_add_eye), "| subjects:", n_distinct(df_add_eye$ID), "\n")

# -----------------------
# Load MODELS + POSTHOC
# -----------------------
posthoc <- readRDS(here("models", "posthoc_results_slope.rds"))
models  <- readRDS(here("models", "all_glmm_models_slope.rds"))

model1 <- models$model1
model2 <- models$model2
model3 <- models$model3
model5 <- models$model5

# ============================================================
# PLOT 1: Model 1 Drug (additional dataset bars) + PLANNED contrasts
# ============================================================
plot_m1_add <- df_all_eye %>%
  group_by(Drug) %>%
  summarise(
    n = sum(!is.na(FixTimePerc)),
    mean = mean(FixTimePerc, na.rm = TRUE),
    sd   = sd(FixTimePerc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Drug = factor(Drug, levels = drug_levels),
    lower = mean - sd,
    upper = mean + sd
  )

emm_m1 <- get_emm_part(posthoc$Model1$drug_contrasts)
drug_lvls_m1 <- levels(emm_m1@grid$Drug)

contrast_list_m1 <- list(
  `M > N` = make_ab_vec(drug_lvls_m1, "morphine", "naltrexone"),
  `M > P` = make_ab_vec(drug_lvls_m1, "morphine", "placebo"),
  `P > N` = make_ab_vec(drug_lvls_m1, "placebo",  "naltrexone")
)

ctr_m1 <- contrast(emm_m1, method = contrast_list_m1)
s_m1 <- as.data.frame(summary(ctr_m1, infer = TRUE))
stat_col_m1 <- if ("t.ratio" %in% names(s_m1)) "t.ratio" else if ("z.ratio" %in% names(s_m1)) "z.ratio" else NA
if (is.na(stat_col_m1)) stop("No t.ratio/z.ratio in Model1 contrast summary.")

m1_contr_df <- s_m1 %>%
  transmute(
    contrast,
    t = as.numeric(.data[[stat_col_m1]]),
    p = as.numeric(p.value)
  )

contrast_to_pairs <- tibble(
  contrast = c("M > N","M > P","P > N"),
  group1   = c("morphine","morphine","placebo"),
  group2   = c("naltrexone","placebo","naltrexone")
)

ymax_m1 <- max(plot_m1_add$upper, na.rm = TRUE)
y_lim_m1 <- ylim0(ymax_m1, extra_top = 0.55)
ypos_m1 <- ymax_m1 + diff(y_lim_m1) * c(0.10, 0.20, 0.30)

pval_m1 <- m1_contr_df %>%
  left_join(contrast_to_pairs, by = "contrast") %>%
  arrange(factor(contrast, levels = c("M > N","M > P","P > N"))) %>%
  mutate(
    y.position = ypos_m1,
    label = paste0(contrast, ": t = ", fmt2(t), p_sig_suffix(p))
  ) %>%
  select(group1, group2, y.position, label)

p_m1_drug_add <- ggplot(plot_m1_add, aes(x = Drug, y = mean, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = y_lim_m1, clip = "off") +
  ggpubr::stat_pvalue_manual(
    pval_m1,
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
    title = "Eye-region fixation time by Drug (Model 1, additional dataset only)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.margin = margin(10, 20, 10, 20))

# ============================================================
# PLOT 2: Model 1 Drug (additional dataset bars) + TUKEY brackets
# ============================================================
pw_m1_tukey <- pairs(emm_m1, adjust = "tukey")
s_link <- as.data.frame(summary(pw_m1_tukey, infer = TRUE))  # link scale

stat_col <- if ("t.ratio" %in% names(s_link)) "t.ratio" else if ("z.ratio" %in% names(s_link)) "z.ratio" else NA
if (is.na(stat_col)) stop("No t.ratio/z.ratio found in Tukey pairwise summary.")
if (!"estimate" %in% names(s_link)) stop("No 'estimate' column found in Tukey pairwise summary.")

abbr <- c(placebo = "P", morphine = "M", naltrexone = "N")

pval_tukey <- s_link %>%
  transmute(
    contrast = as.character(contrast),
    estimate = as.numeric(estimate),
    t_raw = as.numeric(.data[[stat_col]]),
    p = as.numeric(p.value)
  ) %>%
  tidyr::separate(contrast, into = c("g1", "g2"), sep = " - ", remove = FALSE) %>%
  filter(!is.na(g1) & !is.na(g2)) %>%
  mutate(
    flipped = estimate < 0,
    bigger  = ifelse(flipped, g2, g1),
    smaller = ifelse(flipped, g1, g2),
    t = ifelse(flipped, -t_raw, t_raw),
    group1 = bigger,
    group2 = smaller,
    short = paste0(abbr[bigger], " > ", abbr[smaller]),
    label = paste0(short, ": t = ", fmt2(t), p_sig_suffix(p))
  ) %>%
  select(group1, group2, label) %>%
  distinct(group1, group2, .keep_all = TRUE) %>%
  arrange(group1, group2)

ymax_add <- max(plot_m1_add$upper, na.rm = TRUE)
y_lim_add <- ylim0(ymax_add, extra_top = 0.65)

pval_tukey <- pval_tukey %>%
  mutate(y.position = ymax_add + diff(y_lim_add) * (0.12 + 0.10 * (row_number() - 1)))

p_m1_drug_add_tukey <- ggplot(plot_m1_add, aes(x = Drug, y = mean, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = y_lim_add, clip = "off") +
  ggpubr::stat_pvalue_manual(
    pval_tukey,
    label = "label",
    xmin = "group1", xmax = "group2",
    y.position = "y.position",
    tip.length = 0.01,
    bracket.size = 0.6,
    size = 3.4
  ) +
  labs(
    x = NULL,
    y = "Fixation Time (%)",
    title = "Tukey-adjusted contrasts for Drug effect (19 additional participants only)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    plot.margin = margin(10, 25, 10, 25)
  )

# ============================================================
# PLOT 3: H2 (Model 1): ASQ vs eye-region fixation + density inside
# ============================================================
df_eye <- df %>%
  filter(AOI == "eye_brow") %>%
  mutate(
    ID        = factor(as.character(ID)),
    Drug      = factor(Drug, levels = drug_levels),
    FaceGender= factor(FaceGender),
    Gaze2     = factor(Gaze2),
    AttrLevel = factor(AttrLevel),
    Imagelist = factor(Imagelist),
    Session   = factor(Session),
    Genetics  = if ("Genetics" %in% names(.)) factor(Genetics, levels = c("A","G")) else Genetics,
    StimOrder = readr::parse_number(as.character(StimOrder))
  )

mf1 <- model1$frame
fac_vars <- c("Drug","Genetics","FaceGender","Gaze2","AttrLevel","Imagelist","Session","ID","AOI")
for (v in fac_vars) if (v %in% names(mf1) && !is.factor(mf1[[v]])) mf1[[v]] <- factor(mf1[[v]])
if ("Drug" %in% names(mf1)) mf1$Drug <- factor(mf1$Drug, levels = drug_levels)
if ("Genetics" %in% names(mf1)) mf1$Genetics <- factor(mf1$Genetics, levels = c("A","G"))

asq_seq <- seq(
  quantile(df_eye$ASQ, 0.02, na.rm = TRUE),
  quantile(df_eye$ASQ, 0.98, na.rm = TRUE),
  length.out = 100
)

nuis1 <- intersect(c("Drug","Genetics","FaceGender","Gaze2","AttrLevel","Imagelist","Session"), names(mf1))

rg1_asq <- ref_grid(
  model1,
  data = mf1,
  at = list(ASQ = asq_seq, AOI = "eye_brow"),
  nuisance = nuis1,
  type = "response"
)

emm_asq <- emmeans(rg1_asq, ~ ASQ)
pred_df <- as.data.frame(summary(emm_asq, infer = TRUE, type = "response"))

lower_col <- intersect(c("lower.CL", "asymp.LCL", "lower.HPD"), names(pred_df))[1]
upper_col <- intersect(c("upper.CL", "asymp.UCL", "upper.HPD"), names(pred_df))[1]
if (is.na(lower_col) || is.na(upper_col)) {
  stop("Could not find CI columns in emmeans output. Columns: ", paste(names(pred_df), collapse = ", "))
}

pred_df <- pred_df %>%
  transmute(
    ASQ = ASQ,
    fit   = response * 100,
    lower = .data[[lower_col]] * 100,
    upper = .data[[upper_col]] * 100
  )

y_top <- max(pred_df$upper, na.rm = TRUE)
density_height <- 0.18 * y_top

p_H2_density_inside <- ggplot() +
  geom_density(
    data = df_eye,
    aes(x = ASQ, y = after_stat(scaled) * density_height),
    fill = "grey60",
    alpha = 0.35,
    color = NA
  ) +
  geom_ribbon(
    data = pred_df,
    aes(x = ASQ, ymin = lower, ymax = upper),
    alpha = 0.25
  ) +
  geom_line(
    data = pred_df,
    aes(x = ASQ, y = fit),
    linewidth = 1
  ) +
  scale_y_continuous(limits = c(0, y_top * 1.05), expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = "ASQ score",
    y = "Predicted fixation time to eye region (%)",
    title = "Eye-region fixation by ASQ score"
  ) +
  theme_minimal(base_size = 12)

# ============================================================
# PLOT 4: EH4.1 (Model 1): fixation by genotype + OR/effect bracket
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
# PLOT 5: EH4.2 (Model 3): ASQ × Genotype interaction + density
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

# ============================================================
# PLOT 6: Model 2: Drug effects at LOW vs HIGH ASQ (MEDIAN SPLIT)
# Brackets shown ONLY if significant (p < .05); otherwise none
# ============================================================
med_asq <- median(df_all_eye$ASQ, na.rm = TRUE)

df_asq_groups_full <- df_all_eye %>%
  filter(!is.na(ASQ), !is.na(Drug)) %>%     # removes NA panel
  mutate(
    ASQ_level = if_else(ASQ <= med_asq, "Low ASQ (<= median)", "High ASQ (> median)"),
    ASQ_level = factor(ASQ_level, levels = c("Low ASQ (<= median)", "High ASQ (> median)")),
    Drug = factor(Drug, levels = drug_levels)
  )

plot_m2_full <- df_asq_groups_full %>%
  group_by(ASQ_level, Drug) %>%
  summarise(
    n = sum(!is.na(FixTimePerc)),
    mean = ifelse(n > 0, mean(FixTimePerc, na.rm = TRUE), NA_real_),
    sd   = ifelse(n > 1, sd(FixTimePerc,   na.rm = TRUE), NA_real_),
    .groups = "drop"
  ) %>%
  mutate(lower = mean - sd, upper = mean + sd)

high_asq_val <- median(df_all_eye$ASQ[df_all_eye$ASQ > med_asq], na.rm = TRUE)

mf2 <- model2$frame
fac_vars2 <- c("Drug","Genetics","FaceGender","Gaze2","AttrLevel","Imagelist","Session","ID","AOI")
for (v in fac_vars2) if (v %in% names(mf2) && !is.factor(mf2[[v]])) mf2[[v]] <- factor(mf2[[v]])
if ("Drug" %in% names(mf2)) mf2$Drug <- factor(mf2$Drug, levels = drug_levels)
if ("Genetics" %in% names(mf2)) mf2$Genetics <- factor(mf2$Genetics, levels = c("A","G"))

nuis2 <- intersect(c("Genetics","FaceGender","Gaze2","AttrLevel","Imagelist","Session"), names(mf2))

rg2_high <- ref_grid(
  model2, data = mf2,
  at = list(ASQ = high_asq_val, AOI = "eye_brow"),
  nuisance = nuis2,
  type = "response"
)

emm_high <- emmeans(rg2_high, ~ Drug)
drug_lvls_m2 <- levels(emm_high@grid$Drug)

ctr_m2_high <- contrast(
  emm_high,
  method = list("M vs N" = make_ab_vec(drug_lvls_m2, "morphine", "naltrexone"))
)

or2 <- extract_or_p(ctr_m2_high)
is_sig_m2 <- !is.na(or2$p) && (or2$p < 0.05)

y_high <- plot_m2_full %>%
  filter(ASQ_level == "High ASQ (> median)") %>%
  summarise(y = max(upper, na.rm = TRUE)) %>%
  pull(y)

ymax_m2 <- max(plot_m2_full$upper, na.rm = TRUE)
y_lim_m2 <- ylim0(ymax_m2, extra_top = if (is_sig_m2) 0.25 else 0.10)

pval_m2 <- NULL
if (is_sig_m2 && is.finite(y_high)) {
  lab_m2 <- paste0("M vs N: ", or2$label, " = ", fmt2(or2$est), p_sig_suffix(or2$p))
  pval_m2 <- tibble(
    ASQ_level = factor("High ASQ (> median)", levels = levels(plot_m2_full$ASQ_level)),
    group1 = "morphine",
    group2 = "naltrexone",
    y.position = y_high + diff(y_lim_m2) * 0.10,
    label = lab_m2
  )
}

p_m2 <- ggplot(plot_m2_full, aes(x = Drug, y = mean, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  facet_wrap(~ ASQ_level) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = y_lim_m2, clip = "off") +
  { if (!is.null(pval_m2) && nrow(pval_m2) > 0)
      ggpubr::stat_pvalue_manual(
        pval_m2,
        label = "label",
        xmin = "group1", xmax = "group2",
        y.position = "y.position",
        tip.length = 0.01,
        bracket.size = 0.6,
        size = 3.6
      )
  } +
  labs(
    x = NULL,
    y = "Fixation Time (%)",
    title = "Drug effects at low vs high ASQ (ASQ was median-split)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.margin = margin(10, 20, 10, 20))

# ============================================================
# PLOT 7: Model 5 simple effects: Drug differences within genotype at ASQ mean
# ============================================================
plot_m5_full <- df_all_eye %>%
  filter(!is.na(Genetics)) %>%
  group_by(Genetics, Drug) %>%
  summarise(
    n = sum(!is.na(FixTimePerc)),
    mean = mean(FixTimePerc, na.rm = TRUE),
    sd   = sd(FixTimePerc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    Genetics = factor(Genetics, levels = c("A","G")),
    Drug = factor(Drug, levels = drug_levels),
    lower = mean - sd,
    upper = mean + sd
  )

mf5 <- model5$frame
for (v in fac_vars) if (v %in% names(mf5) && !is.factor(mf5[[v]])) mf5[[v]] <- factor(mf5[[v]])
if ("Drug" %in% names(mf5)) mf5$Drug <- factor(mf5$Drug, levels = drug_levels)
if ("Genetics" %in% names(mf5)) mf5$Genetics <- factor(mf5$Genetics, levels = c("A","G"))

nuis5 <- intersect(c("FaceGender","Gaze2","AttrLevel","Imagelist","Session"), names(mf5))

rg5 <- ref_grid(
  model5,
  data = mf5,
  at = list(ASQ = ASQ_mean_full, AOI = "eye_brow"),
  nuisance = nuis5,
  type = "response"
)

emm5 <- emmeans(rg5, ~ Drug | Genetics)
drug_lvls_5 <- levels(emm5@grid$Drug)

ctr5 <- contrast(
  emm5,
  method = list("M vs P" = make_ab_vec(drug_lvls_5, "morphine", "placebo")),
  by = "Genetics"
)

s5 <- as.data.frame(summary(ctr5, infer = TRUE, type = "response"))
rowG <- s5 %>% filter(Genetics == "G") %>% slice(1)
if (nrow(rowG) == 0) stop("Could not find Genetics == 'G' in model5 contrast output.")

est_col5 <- intersect(c("odds.ratio","ratio","response","estimate"), names(rowG))[1]
if (is.na(est_col5)) stop("No estimate column in model5 contrast output.")
labtype5 <- if (est_col5 %in% c("odds.ratio","ratio")) "OR" else "effect"

lab_m5 <- label_effect_p("M vs P", labtype5, as.numeric(rowG[[est_col5]]), as.numeric(rowG$p.value))

yG <- plot_m5_full %>% filter(Genetics == "G") %>% summarise(y = max(upper, na.rm = TRUE)) %>% pull(y)

ymax_m5 <- max(plot_m5_full$upper, na.rm = TRUE)
y_lim_m5 <- ylim0(max(ymax_m5, yG * 1.25), extra_top = 0.25)

pval_m5 <- tibble(
  Genetics = factor("G", levels = levels(plot_m5_full$Genetics)),
  group1 = "morphine",
  group2 = "placebo",
  y.position = yG + diff(y_lim_m5) * 0.10,
  label = lab_m5
)

p_m5 <- ggplot(plot_m5_full, aes(x = Drug, y = mean, fill = Drug)) +
  geom_col(width = 0.7) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.15, linewidth = 0.6) +
  facet_wrap(~ Genetics) +
  scale_fill_manual(values = drug_cols, drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(ylim = y_lim_m5, clip = "off") +
  ggpubr::stat_pvalue_manual(
    pval_m5,
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
    title = "Simple effects: Drug differences within genotype (ASQ at mean, Model 5, full dataset)"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        plot.margin = margin(10, 20, 10, 20))

# ============================================================
# SAVE PLOTS
# ============================================================
ggsave(file.path(out_dir, "model1_drug_fixation_additional_raw_meanSD.png"),
       p_m1_drug_add, width = 7.5, height = 5.5, dpi = 300)

ggsave(file.path(out_dir, "model1_drug_fixation_additional_raw_meanSD_TUKEY_add.png"),
       p_m1_drug_add_tukey, width = 8, height = 6, dpi = 300)

ggsave(file.path(out_dir, "H2_ASQ_vs_eye_fixation_model1_density_inside.png"),
       p_H2_density_inside, width = 8, height = 5.5, dpi = 300)

ggsave(file.path(out_dir, "model1_genotype_fixation_EH4_1.png"),
       p_EH41, width = 7.5, height = 5.5, dpi = 300)

ggsave(file.path(out_dir, "EH4_2_ASQ_by_Genotype_interaction_model3.png"),
       p_EH42, width = 8.5, height = 5.5, dpi = 300)

ggsave(file.path(out_dir, "model2_drug_by_asq_median_split.png"),
       p_m2, width = 8.5, height = 5.5, dpi = 300)

ggsave(file.path(out_dir, "model5_simple_effects_drug_within_genotype_ASQmean.png"),
       p_m5, width = 9, height = 5.5, dpi = 300)

cat("Saved plots to:\n", out_dir, "\n")