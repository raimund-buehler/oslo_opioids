library(tidyverse)
library(here)
library(lmerTest)

path <- here("data", "analyses", "df_fix_time_perc.csv")

df <- read_csv(path)

# order factor levels
df$Drug <- factor(df$Drug, levels = c("morphine", "placebo", "naltrexone"), ordered = T)
df$AOI <- factor(df$AOI, levels = c("eye_brow", "nose_mouth_jaw", "forehead_cheek"), ordered = T)

# filter out whitespace_hair, separate by gender and filter first 130 participants
df_f <- df %>%
    filter(AOI != "whitespace_hair", FaceGender == "F", ID %in% c(101:130)) %>%
    group_by(ID, AOI) %>%
    # Add centered variable to remove between-subject variance
    mutate(FixTimePerc_centered = FixTimePerc - mean(FixTimePerc, na.rm = TRUE)) %>%
    ungroup()

df_m <- df %>%
    filter(AOI != "whitespace_hair", FaceGender == "M", ID %in% c(101:130)) %>%
    group_by(ID, AOI) %>%
    # Add centered variable to remove between-subject variance
    mutate(FixTimePerc_centered = FixTimePerc - mean(FixTimePerc, na.rm = TRUE)) %>%
    ungroup()

# filter outliers (likely not the reason for different SD's): check min and max
# df_f %>%
#     group_by(Drug, AOI) %>%
#     summarise(
#         FixTimePerc_min = min(FixTimePerc, na.rm = T),
#         FixTimePerc_max = max(FixTimePerc, na.rm = T)
#     )

# df_f <- df_f %>% filter(!FixTimePerc > 75)

# create barpolot of fix_t% per Drug
# barplot function
barplot <- function(df) {
    df %>%
        group_by(Drug, AOI) %>%
        summarise(FixTimePerc_mean = mean(FixTimePerc, na.rm = T)) %>%
        ggplot(aes(x = AOI, y = FixTimePerc_mean, fill = Drug)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_y_continuous(breaks = 0:60) +
        labs(title = "FixTimePerc per Drug")
}

barplot(df_f)
barplot(df_m)

# means and sd, sd from centered
means_sds_df <- function(df) {
    df %>%
        filter(AOI == "eye_brow") %>%
        group_by(Drug) %>%
        summarise(
            FixTimePerc_mean = mean(FixTimePerc, na.rm = TRUE),
            FixTimePerc_sd = sd(FixTimePerc_centered, na.rm = TRUE),
            FixTimePerc_se = FixTimePerc_sd / sqrt(n()),
            CI_lower = FixTimePerc_mean - qt(0.975, df = n() - 1) * FixTimePerc_se,
            CI_upper = FixTimePerc_mean + qt(0.975, df = n() - 1) * FixTimePerc_se
        )
}

# females
means_sds_df(df_f)
# means pretty close, sd's within subject! --> mean centered
# published: female (Eye Region, M = 45.08 SD = 15.18; P = 41.89, SD = 16.42; N = 39.17, SD = 18.22)

# males
means_sds_df(df_m)
# same
# published: male (Eye Region, M = 40.64, SD = 15.52; P: 39.51, SD = 16.35; N: 36.21, SD = 17.73).

# MODELING

# fixed: this is probably the analysis that was published
fix_perc_f <- lm(FixTimePerc ~ AOI * Drug * Gaze2 * AttrLevel + StimOrder + Imagelist + Session, data = df_f)
fix_perc_m <- lm(FixTimePerc ~ AOI * Drug * Gaze2 * AttrLevel + StimOrder + Imagelist + Session, data = df_m)

anova(fix_perc_f) # AOI*Drug F(4,5301) = 22.07
# published: female AOI*Drug F(4,5279) = 22.44, P < 0.001]

anova(fix_perc_m) # AOI*Drug F(4,5301) = 11.86
# published: male   AOI*Drug F(4,5266) = 12.29, P < 0.001

# random intercept: only significant for females
fix_perc_f <- lmer(FixTimePerc ~ AOI * Drug * Gaze2 * AttrLevel + StimOrder + Imagelist + Session + (1 | ID), data = df_f)
fix_perc_m <- lmer(FixTimePerc ~ AOI * Drug * Gaze2 * AttrLevel + StimOrder + Imagelist + Session + (1 | ID), data = df_m)

anova(fix_perc_f)
anova(fix_perc_m)

# random slope for AOI and Drug: only significant for females
fix_perc_f <- lmer(FixTimePerc ~ AOI * Drug * Gaze2 * AttrLevel + StimOrder + Imagelist + Session + (Drug | ID), data = df_f)
fix_perc_m <- lmer(FixTimePerc ~ AOI * Drug * Gaze2 * AttrLevel + StimOrder + Imagelist + Session + (Drug | ID), data = df_m)

anova(fix_perc_f)
anova(fix_perc_m)
