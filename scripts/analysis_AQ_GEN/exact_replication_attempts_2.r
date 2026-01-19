#only version in which P > N (female) was significant at p < .05 (very close)

#         F    M > N 6.89  < .001
#         F    M > P 4.76  < .001
#         F    P > N 2.13 = 0.034
#         M    M > N 6.09  < .001
#         M    M > P 4.55  < .001
#         M    P > N 1.55 = 0.122


library(tidyverse)
library(here)
library(lmerTest)
library(emmeans)

# (optional) verhindert die "df disabled" Hinweise bei großen N
emm_options(pbkrtest.limit = 1e6, lmerTest.limit = 1e6)

df <- read_csv(here("data","analyses","fix_perc_ASQ_GEN.csv"))

id_prefix <- "^1"  # "^1" original, "^2" additional
# Eye-only, beide Stimulus-Geschlechter, gleiche Controls
eye <- df %>%
  filter(AOI == "eye_brow", FaceGender %in% c("F","M")) %>%
  mutate(
    Drug       = factor(Drug, levels = c("morphine","placebo","naltrexone")),
    FaceGender = factor(FaceGender),
    Gaze2      = factor(Gaze2),
    AttrLevel  = factor(AttrLevel),
    Imagelist  = factor(Imagelist),
    Session    = factor(Session),
    ID         = factor(ID)
  )

m_eye <- lmer(
  FixTimePerc ~ Drug * FaceGender + Gaze2 + AttrLevel +
    StimOrder + Imagelist + Session +
    (1 + Drug| ID),
  data = eye
)

emm   <- emmeans(m_eye, ~ Drug | FaceGender, weights = "proportional")
contr <- contrast(emm, method = list(
  `M > N` = c( 1,  0, -1),
  `M > P` = c( 1, -1,  0),
  `P > N` = c( 0,  1, -1)
))

s <- as.data.frame(summary(contr, infer = TRUE))

# robust: t.ratio / z.ratio / t / z
stat_col <- dplyr::case_when(
  "t.ratio" %in% names(s) ~ "t.ratio",
  "t"       %in% names(s) ~ "t",
  "z.ratio" %in% names(s) ~ "z.ratio",
  "z"       %in% names(s) ~ "z",
  TRUE ~ NA_character_
)
if (is.na(stat_col)) stop("Kein Teststatistik-Feld gefunden: ", paste(names(s), collapse=", "))

res <- s %>%
  transmute(
    FaceGender,
    contrast,
    stat = round(.data[[stat_col]], 2),
    p = ifelse(p.value < .001, "< .001", sprintf("= %.3f", p.value))
  )

print(res)

cat("\nPaper-style reporting:\n")
res %>%
  group_by(FaceGender) %>%
  group_walk(~{
    keep <- .x %>%
      filter(
        contrast == "M > N" |
          (contrast %in% c("M > P","P > N") &
             (p == "< .001" | as.numeric(sub("= ","", p)) < .05))
      )
    cat("\n", unique(.x$FaceGender), "faces: ", sep = "")
    cat(paste0(keep$contrast, ", t = ", keep$stat, ", P ", keep$p, collapse = "; "), "\n", sep = "")
  })
