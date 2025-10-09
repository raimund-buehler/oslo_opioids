library(tidyverse)
library(here)
library(readxl)
library(writexl)

path <- here("data", "analyses", "df_fix_n.csv")

df <- read_csv(path)

# df <- df_AOIgroups %>% filter(AOI == "eye_brow")

# change order of Drug levels, morphine, placebo, naltrexone
df$Drug <- factor(df$Drug, levels = c("morphine", "placebo", "naltrexone"), ordered = T)

df_f <-
    df %>%
    filter(AOI == "face", FaceGender == "F", ID %in% c(101:130))

df_m <-
    df %>%
    filter(AOI == "face", FaceGender == "M", ID %in% c(101:130))

# outlier check: filter those with FixCount > 20, doesn't change
# df_f <- df_f %>%
#     filter(!FixCount > 20) %>%
#     select(ID, Drug, StimOrder, Stimulus, AOI, FixCount)

# create barpolot of FixCount per Drug
df_f %>%
    group_by(Drug) %>%
    summarise(FixCount_mean = mean(FixCount, na.rm = T)) %>%
    ggplot(aes(x = Drug, y = FixCount_mean, fill = Drug)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(breaks = 0:20) +
    labs(title = "FixCount per Drug")

df_m %>%
    group_by(Drug) %>%
    summarise(FixCount_mean = mean(FixCount, na.rm = T)) %>%
    ggplot(aes(x = Drug, y = FixCount_mean, fill = Drug)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(breaks = 0:20) +
    labs(title = "FixCount per Drug")

# means and sds
df_f %>%
    group_by(Drug) %>%
    summarise(
        FixCount_mean = mean(FixCount, na.rm = TRUE),
        FixCount_sd = sd(FixCount, na.rm = TRUE)
    )

df_m %>%
    group_by(Drug) %>%
    summarise(
        FixCount_mean = mean(FixCount, na.rm = TRUE),
        FixCount_sd = sd(FixCount, na.rm = TRUE)
    )
# mean and sds are off, but general shape matches

# Modeling

fix_n_f <- lm(FixTimePerc ~ Drug * Gaze2 * AttrLevel + StimOrder + Imagelist + Session, data = df_f)
fix_n_m <- lm(FixTimePerc ~ Drug * Gaze2 * AttrLevel + StimOrder + Imagelist + Session, data = df_m)

anova(fix_n_f)
anova(fix_n_m)

library(lmerTest)

fix_n_f <- lmer(FixTimePerc ~ Drug * Gaze2 * AttrLevel + StimOrder + Imagelist + Session + (1 + Drug | ID), data = df_f)
fix_n_m <- lmer(FixTimePerc ~ Drug * Gaze2 * AttrLevel + StimOrder + Imagelist + Session + (1 + Drug | ID), data = df_m)

anova(fix_n_f)
anova(fix_n_m)
