# Load packages
library(dplyr)
library(readr)

#Working directory
setwd("C:/Users/priva/Documents/GitHub/oslo_opioids/data/analyses")


# Import data
df <- read_csv("fix_perc_ASQ_GEN.csv")

df <- readr::read_csv(
  "fix_perc_ASQ_GEN.csv",
  col_types = cols(
    Stimulus    = col_character(),
    AOI         = col_character(),
    Drug        = col_character(),
    Gaze2       = col_character(),
    FaceGender  = col_character(),
    Genetics    = col_character(),
    ID          = col_double(),
    StimOrder   = col_double(),
    AttrLevel   = col_double(),
    Imagelist   = col_double(),
    Session     = col_double(),
    FixCount    = col_double(),
    FixTime     = col_double(),
    FixTimePerc = col_double(),
    TotalFixTime = col_double(),
    ASQ         = col_double()
  )
)


library(dplyr)
library(readr)



# ================================================================
# 1. PARTICIPANT-LEVEL DATA (one row per ID)
# ================================================================

df_subj <- df %>%
  group_by(ID) %>%
  summarise(
    Genetics = first(Genetics),
    ASQ = first(ASQ),
    
  )


# ================================================================
# 2. PARTICIPANT-LEVEL DESCRIPTIVES
# ================================================================


# ---- ASQ ----
asq_desc_ID <- df_subj %>%
  summarise(
    mean_ASQ = mean(ASQ, na.rm = TRUE),
    sd_ASQ   = sd(ASQ, na.rm = TRUE),
    n        = n()
  )
print(asq_desc_ID)

# ---- Genotype (A vs G alleles per participant) ----
genotype_counts_ID <- df_subj %>%
  count(Genetics) %>%
  mutate(percent = round(100 * n / sum(n), 2))
print(genotype_counts_ID)

# Only G percentage
print(genotype_counts_ID %>% filter(Genetics == "G"))


# ================================================================
# 3. TRIAL-LEVEL DESCRIPTIVES (all rows)
# ================================================================

# ---- Fixation Time ----
fix_desc <- df %>%
  summarise(
    mean_fixPerc = mean(FixTimePerc, na.rm = TRUE),
    sd_fixPerc   = sd(FixTimePerc, na.rm = TRUE),
    mean_totalFT = mean(TotalFixTime, na.rm = TRUE),
    sd_totalFT   = sd(TotalFixTime, na.rm = TRUE),
    n = n()
  )
print(fix_desc)

# ---- AOI Counts ----
aoi_counts <- df %>%
  count(AOI) %>%
  mutate(percent = round(100 * n / sum(n), 2))
print(aoi_counts)

# ---- Fixation by AOI ----
aoi_fix_desc <- df %>%
  group_by(AOI) %>%
  summarise(
    mean_fixPerc = mean(FixTimePerc, na.rm = TRUE),
    sd_fixPerc   = sd(FixTimePerc, na.rm = TRUE),
    mean_totalFT = mean(TotalFixTime, na.rm = TRUE),
    sd_totalFT   = sd(TotalFixTime, na.rm = TRUE),
    n = n()
  )
print(aoi_fix_desc)



library(dplyr)
library(knitr)
library(kableExtra)

library(dplyr)
library(knitr)

# ===========================================================
# 1. Participant-level rows (NO AGE)
# ===========================================================

geno_A <- genotype_counts_ID %>% filter(Genetics == "A")
geno_G <- genotype_counts_ID %>% filter(Genetics == "G")

participant_rows <- tibble(
  Variable = c(
    "ASQ score",
    "Genotype: A/A",
    "Genotype: A/G"
  ),
  Mean = c(
    asq_desc_ID$mean_ASQ,
    NA,
    NA
  ),
  SD = c(
    asq_desc_ID$sd_ASQ,
    NA,
    NA
  ),
  N = c(
    asq_desc_ID$n,
    geno_A$n,
    geno_G$n
  ),
  Percent = c(
    NA,
    geno_A$percent,
    geno_G$percent
  )
)

# ===========================================================
# 2. Trial-level rows
# ===========================================================

trial_rows <- tibble(
  Variable = c(
    "Fixation Time Percentage (%)",
    "Total Fixation Time (ms)"
  ),
  Mean = c(
    fix_desc$mean_fixPerc,
    fix_desc$mean_totalFT
  ),
  SD = c(
    fix_desc$sd_fixPerc,
    fix_desc$sd_totalFT
  ),
  N = c(
    fix_desc$n,
    fix_desc$n
  ),
  Percent = c(NA, NA)
)

# ===========================================================
# 3. AOI rows
# ===========================================================

aoi_rows <- aoi_fix_desc %>%
  transmute(
    Variable = paste0("AOI: ", AOI),
    Mean = mean_fixPerc,
    SD = sd_fixPerc,
    N = n,
    Percent = NA
  )

# ===========================================================
# 4. Combine all rows
# ===========================================================

apa_table <- bind_rows(
  participant_rows,
  trial_rows,
  aoi_rows
)

# Round all numeric columns in the table to two decimals
apa_table <- apa_table %>%
  mutate(
    Mean = round(Mean, 2),
    SD = round(SD, 2),
    Percent = round(Percent, 2)
  )


# ===========================================================
# 5. Print in APA style (simple)
# ===========================================================

kable(
  apa_table,
  caption = "Table 1. Descriptive Statistics for Participant-Level, Trial-Level, and AOI-Level Variables (APA Style)",
  format = "markdown",
  digits = 2,
  align = "lcccc"
)

library(officer)
library(flextable)

# Convert APA table to a flextable object
ft <- flextable(apa_table)
ft <- autofit(ft)

# Add title in APA style
doc <- read_docx()
doc <- body_add_par(doc, "Table 1. Descriptive Statistics for Participant-Level, Trial-Level, and AOI-Level Variables", style = "heading 2")
doc <- body_add_flextable(doc, ft)

# Save as Word file
print(doc, target = "APA_Descriptive_Table.docx")
