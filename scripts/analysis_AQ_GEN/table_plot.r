# ============================================================
# Model 4 – Random Intercept vs Random Slope + LRTs + APA Table
# (fixed: VarCorr extraction + fit_row tibble size + slope vars,
#         robust emmeans plotting columns)
# ============================================================

library(dplyr)
library(glmmTMB)
library(emmeans)
library(broom.mixed)
library(officer)
library(flextable)
library(ggplot2)

# -----------------------
# 1) Load models from .rds
# -----------------------
path_int   <- "C:/Users/priva/Documents/GitHub/oslo_opioids/models/all_glmm_models_intercept.rds"
path_slope <- "C:/Users/priva/Documents/GitHub/oslo_opioids/models/all_glmm_models_slope.rds"

# quick diagnostics
cat("Working directory:\n", getwd(), "\n\n")
cat("Intercept file exists? ", file.exists(path_int), "\n")
cat("Slope file exists?     ", file.exists(path_slope), "\n\n")

if (!file.exists(path_int)) {
  stop("Intercept .rds not found at: ", path_int,
       "\nTip: check spelling, repo folder name, or whether it's under OneDrive.")
}
if (!file.exists(path_slope)) {
  stop("Slope .rds not found at: ", path_slope,
       "\nTip: check spelling, repo folder name, or whether it's under OneDrive.")
}

models_int <- readRDS(path_int)
models_slope <- readRDS(path_slope)

# sanity check: what did we load?
cat("Loaded objects:\n")
print(names(models_int))
print(names(models_slope))
m4_int   <- models_int$model4
m4_slope <- models_slope$model4

# -----------------------
# 2) Refit reduced models for LRTs (fixed-effects LRT)
#    Key test: Drug:Genetics interaction
# -----------------------
m4_int_red   <- update(m4_int,   . ~ . - Drug:Genetics)
m4_slope_red <- update(m4_slope, . ~ . - Drug:Genetics)

lrt_int   <- anova(m4_int,   m4_int_red)
lrt_slope <- anova(m4_slope, m4_slope_red)

# -----------------------
# 3) LRT: Random Slope needed?
#    Compare RI vs RS for same fixed part
# -----------------------
lrt_ri_vs_rs <- anova(m4_int, m4_slope)

# -----------------------
# 4) Helper: extract fixed effects (APA-ready)
# -----------------------
fixef_table <- function(model, model_label) {
  s <- summary(model)$coefficients$cond %>%
    as.data.frame() %>%
    tibble::rownames_to_column("Term") %>%
    transmute(
      Model = model_label,
      Term,
      b  = Estimate,
      SE = `Std. Error`,
      z  = `z value`,
      p  = `Pr(>|z|)`
    )
  s
}

fe_int   <- fixef_table(m4_int,   "Random intercept")
fe_slope <- fixef_table(m4_slope, "Random slope (Drug|ID)")

keep_terms <- c(
  "Drugmorphine", "Drugnaltrexone", "GeneticsG",
  "Drugmorphine:GeneticsG", "Drugnaltrexone:GeneticsG",
  "ASQ"
)

fe_int_k   <- fe_int   %>% filter(Term %in% keep_terms)
fe_slope_k <- fe_slope %>% filter(Term %in% keep_terms)

round_df <- function(df) {
  df %>%
    mutate(
      b  = round(b, 3),
      SE = round(SE, 3),
      z  = round(z, 2),
      p  = ifelse(is.na(p), NA, ifelse(p < .001, "<.001", sprintf("%.3f", p)))
    )
}

fe_int_k   <- round_df(fe_int_k)
fe_slope_k <- round_df(fe_slope_k)

# -----------------------
# 5) VarCorr helpers (ROBUST)
#    Avoid hard-coding $ID and avoid tibble size mismatch.
# -----------------------
get_re_block <- function(model) {
  vc_all <- VarCorr(model)$cond
  grp <- names(vc_all)
  if (length(grp) == 0) stop("No conditional random effects found in VarCorr(model)$cond")
  vc_all[[grp[1]]]  # take first random-effect grouping factor
}

get_re_group_name <- function(model) {
  vc_all <- VarCorr(model)$cond
  grp <- names(vc_all)
  if (length(grp) == 0) stop("No conditional random effects found in VarCorr(model)$cond")
  grp[1]
}

fit_row <- function(model, model_label) {
  vc <- get_re_block(model)
  grp <- get_re_group_name(model)
  sdvec <- attr(vc, "stddev")  # intercept first

  tibble::tibble(
    Model = model_label,
    Term  = c("AIC", "logLik", paste0("Var(", grp, " intercept)")),
    b     = c(AIC(model), as.numeric(logLik(model)), sdvec[1]^2),
    SE    = NA_real_,
    z     = NA_real_,
    p     = NA_character_
  ) %>%
    mutate(b = round(b, 3))
}

add_slope_vars <- function(model, model_label) {
  vc <- get_re_block(model)
  grp <- get_re_group_name(model)
  sdvec <- attr(vc, "stddev")

  # only intercept present -> nothing to add
  if (length(sdvec) <= 1) return(tibble::tibble())

  sd_slopes <- sdvec[-1]  # drop intercept (already reported)
  nm <- names(sd_slopes)
  if (is.null(nm) || any(nm == "")) nm <- paste0("slope", seq_along(sd_slopes))

  tibble::tibble(
    Model = model_label,
    Term  = paste0("Var(", grp, " ", nm, ")"),
    b     = round(sd_slopes^2, 3),
    SE    = NA_real_,
    z     = NA_real_,
    p     = NA_character_
  )
}

fit_int   <- fit_row(m4_int,   "Random intercept")
fit_slope <- fit_row(m4_slope, "Random slope (Drug|ID)")

slope_vars <- add_slope_vars(m4_slope, "Random slope (Drug|ID)")

# -----------------------
# 6) LRT table
# -----------------------
lrt_to_tbl <- function(a, label) {
  out <- as.data.frame(a)
  tibble::tibble(
    Test = label,
    Chisq = round(out$Chisq[nrow(out)], 2),
    df    = out$Df[nrow(out)],
    p     = ifelse(out$`Pr(>Chisq)`[nrow(out)] < .001,
                   "<.001",
                   sprintf("%.3f", out$`Pr(>Chisq)`[nrow(out)]))
  )
}

lrt_tbl <- bind_rows(
  lrt_to_tbl(lrt_int,     "LRT: Drug×Genetics (RI)"),
  lrt_to_tbl(lrt_slope,   "LRT: Drug×Genetics (RS)"),
  lrt_to_tbl(lrt_ri_vs_rs,"LRT: Random slope vs intercept")
)

# -----------------------
# 7) Build APA table (wide comparison)
# -----------------------
apa_fixed <- bind_rows(fe_int_k, fe_slope_k) %>%
  arrange(
    factor(Model, levels = c("Random intercept", "Random slope (Drug|ID)")),
    match(Term, keep_terms)
  )

apa_fixed_wide <- apa_fixed %>%
  mutate(value = paste0(b, " (", SE, "), z=", z, ", p=", p)) %>%
  select(Model, Term, value) %>%
  tidyr::pivot_wider(names_from = Model, values_from = value) %>%
  rename(Effect = Term)

apa_other <- bind_rows(fit_int, fit_slope, slope_vars) %>%
  mutate(value = as.character(b)) %>%
  select(Model, Term, value) %>%
  tidyr::pivot_wider(names_from = Model, values_from = value) %>%
  rename(Effect = Term)

apa_table <- bind_rows(
  tibble::tibble(Effect = "Fixed effects", `Random intercept` = "", `Random slope (Drug|ID)` = ""),
  apa_fixed_wide,
  tibble::tibble(Effect = "Model fit / random effects", `Random intercept` = "", `Random slope (Drug|ID)` = ""),
  apa_other
)

# -----------------------
# 8) Export APA table to Word (less stretched)
# -----------------------
ft <- flextable(apa_table) %>%
  fontsize(size = 10, part = "all") %>%
  autofit() %>%
  set_table_properties(layout = "autofit", width = 0.95) %>%
  align(align = "left", part = "all") %>%
  valign(valign = "top", part = "all")

doc <- read_docx()
doc <- body_add_par(doc,
  "Table X. Model 4 (Drug × Genetics): Random-intercept vs Random-slope models",
  style = "heading 2"
)
doc <- body_add_flextable(doc, ft)

doc <- body_add_par(doc, "Likelihood-ratio tests (LRTs)", style = "heading 2")
ft_lrt <- flextable(lrt_tbl) %>%
  autofit() %>%
  set_table_properties(layout = "autofit", width = 0.6)
doc <- body_add_flextable(doc, ft_lrt)

print(doc, target = "APA_Model4_Intercept_vs_Slope.docx")

# -----------------------
# 9) Plot: interaction plot Drug × Genetics (response scale) -- ROBUST
# -----------------------
emm <- emmeans(m4_int, ~ Drug * Genetics)
emm_df <- as.data.frame(summary(emm, type = "response"))

# response column can differ by emmeans/model family
if ("response" %in% names(emm_df)) {
  emm_df$y <- emm_df$response
} else if ("prob" %in% names(emm_df)) {
  emm_df$y <- emm_df$prob
} else if ("rate" %in% names(emm_df)) {
  emm_df$y <- emm_df$rate
} else {
  stop("Couldn't find a response-scale column in emmeans output (expected response/prob/rate).")
}

# CI column names can differ
if (all(c("lower.CL", "upper.CL") %in% names(emm_df))) {
  emm_df$ymin <- emm_df$lower.CL
  emm_df$ymax <- emm_df$upper.CL
} else if (all(c("asymp.LCL", "asymp.UCL") %in% names(emm_df))) {
  emm_df$ymin <- emm_df$asymp.LCL
  emm_df$ymax <- emm_df$asymp.UCL
} else if (all(c("LCL", "UCL") %in% names(emm_df))) {
  emm_df$ymin <- emm_df$LCL
  emm_df$ymax <- emm_df$UCL
} else {
  stop("Couldn't find CI columns in emmeans output (lower.CL/upper.CL or asymp.LCL/asymp.UCL).")
}

p <- ggplot(emm_df, aes(x = Drug, y = y, group = Genetics, color = Genetics)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_line(position = position_dodge(width = 0.2)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax),
                width = 0.1, position = position_dodge(width = 0.2)) +
  labs(
    x = "Drug condition",
    y = "Predicted fixation proportion (eye region)",
    title = "Model 4: Drug × Genotype interaction (predicted means, 95% CI)"
  ) +
  theme_minimal()

ggsave("Model4_Drug_by_Genotype_plot.png", plot = p, width = 7, height = 5, dpi = 300)
