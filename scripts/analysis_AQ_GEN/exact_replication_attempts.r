#numerically closesest to reporting of original study
# but P > N (female) is not significant

#          F    M > N 5.33 < 0.001
#          F    M > P 3.50 < 0.001
#          F    P > N 1.78 = 0.075

#          M    M > N 4.10 < 0.001
#          M    M > P 1.66 = 0.097
#          M    P > N 2.40 = 0.016


library(tidyverse)
library(here)
library(lme4)
library(lmerTest)  # gives Type III F tables nicely
library(emmeans)

# SPSS-like Type III needs sum-to-zero contrasts
options(contrasts = c("contr.sum", "contr.poly"))

df <- read_csv(here("data","analyses","fix_perc_ASQ_GEN.csv"))

id_prefix <- "^1"  # "^1" original, "^2" additional

dat <- df %>%
  filter(grepl(id_prefix, as.character(ID))) %>%
  filter(FaceGender %in% c("F","M")) %>%
  filter(AOI %in% c("eye_brow","nose_mouth_jaw","forehead_cheek")) %>%   # 3 AOIs -> df=4 for AOI×Drug
  mutate(
    ID         = factor(ID),
    Drug       = factor(Drug, levels = c("morphine","placebo","naltrexone")),
    AOI        = factor(AOI, levels = c("eye_brow","nose_mouth_jaw","forehead_cheek")),
    Gaze2      = factor(Gaze2),
    AttrLevel  = factor(AttrLevel),
    Imagelist  = factor(Imagelist),
    Session    = factor(Session),
    StimOrder  = as.numeric(StimOrder)
  )

contrast_list <- list(
  `M > N` = c( 1,  0, -1),
  `M > P` = c( 1, -1,  0),
  `P > N` = c( 0,  1, -1)
)

label_fg <- function(x) ifelse(x == "F", "female", "male")

run_facegender <- function(face) {

  d <- dat %>% filter(FaceGender == face)

  cat("\n", paste0(rep("=", 76), collapse=""), "\n", sep="")
  cat("Stimulus FaceGender =", face, "| N_subj =", n_distinct(d$ID), "| N_obs =", nrow(d), "\n")
  cat(paste0(rep("=", 76), collapse=""), "\n", sep="")

  # --- Model for contrasts (ML, as in their method text) ---
  m_ml <- lmer(
    FixTimePerc ~ AOI * Drug * Gaze2 * AttrLevel +
      StimOrder + Imagelist + Session +
      (1 + Drug | ID),
    data = d,
    REML = FALSE
  )

  # --- Same model refit with REML for F-tests (required for F in this setup) ---
  m_reml <- update(m_ml, REML = TRUE)

  # Type III ANOVA table with F tests (lmerTest)
  a3 <- anova(m_reml, type = 3) %>% as.data.frame() %>% tibble::rownames_to_column("Effect")
  aoi_drug <- a3 %>% filter(Effect == "AOI:Drug")

  cat("\nType III test (AOI × Drug) from REML fit:\n")
  print(aoi_drug, row.names = FALSE)

  # Eye-region planned contrasts derived FROM the AOI model (AOI fixed at eye)
  emm_eye <- emmeans(m_ml, ~ Drug, at = list(AOI = "eye_brow"), weights = "proportional")
  contr   <- contrast(emm_eye, method = contrast_list)
  s       <- as.data.frame(summary(contr, infer = TRUE))

  # statistic column (t.ratio usually)
  stat_col <- if ("t.ratio" %in% names(s)) "t.ratio" else if ("z.ratio" %in% names(s)) "z.ratio" else stop(paste(names(s), collapse=", "))

  res <- s %>%
    transmute(
      FaceGender = face,
      contrast,
      t = round(.data[[stat_col]], 2),
      P = ifelse(p.value < .001, "< 0.001", sprintf("= %.3f", p.value))
    )

  cat("\nEye region planned contrasts (from ML AOI model):\n")
  print(res, row.names = FALSE)

  cat("\nPaper-style reporting:\n")
  keep <- res %>%
    filter(
      contrast == "M > N" |
        (contrast %in% c("M > P","P > N") &
           (P == "< 0.001" | as.numeric(sub("= ","", P)) < .05))
    )

  cat(label_fg(face), "faces: ", paste0(keep$contrast, ", t = ", keep$t, ", P ", keep$P, collapse="; "), "\n", sep="")

  invisible(list(aoi_drug = aoi_drug, contrasts = res))
}

out_F <- run_facegender("F")
out_M <- run_facegender("M")
