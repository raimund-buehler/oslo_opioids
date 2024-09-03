library(tidyverse)
library(here)
library(readxl)
library(writexl)

path <- here("data", "8_merged_session.csv")
# path <- here("data", "AllDrugs_AllAOIs.xlsx")

df <- read_csv(path) %>%
    rename(
        AOI = "Area of Interest",
        FixCount = "Fixation Count",
        FixTime = "Fixation Time [ms]",
        FixTimePerc = "Fixation Time [%]"
    )

df <- df %>%
    select(
        StimOrder,
        ID,
        Stimulus,
        AOI,
        FixCount,
        FixTime,
        FixTimePerc,
        Drug:Session
    )

# How many AOI per Subject and Stim
df %>%
    group_by(ID, Session, Stimulus, StimOrder) %>%
    summarise(n = n()) %>%
    filter(n != 11)

# Collapse Eyes and Cheeks and sum relevant columns
df_collapsed <- df %>%
    mutate(AOI = case_when(
        AOI %in% c("R_cheek", "L_cheek") ~ "Cheeks",
        AOI %in% c("R_eye", "L_eye") ~ "Eyes",
        TRUE ~ AOI
    )) %>%
    group_by(ID, Stimulus, StimOrder, AOI, Drug, AttrLevel, Gaze2, FaceGender, Imagelist, Session) %>%
    summarise(
        FixCount = sum(FixCount, na.rm = TRUE),
        FixTime = sum(FixTime, na.rm = TRUE),
        FixTimePerc = sum(FixTimePerc, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(ID, Session, StimOrder)

# How many AOI per Subject and Stim
df_collapsed %>%
    group_by(ID, Session, Stimulus, StimOrder) %>%
    summarise(n = n()) %>%
    filter(n != 9)

# Identify the specific AOIs present in the problematic cases
missing_AOIs <- df_collapsed %>%
    group_by(ID, Session, Stimulus, StimOrder) %>%
    filter(n() != 9) %>%
    summarise(
        missing_AOIs = paste(unique(AOI), collapse = ", "),
        count = n()
    ) %>%
    arrange(desc(count))

# Filter the original dataframe to see the entries for the problematic cases
problematic_cases <- df %>%
    filter(ID %in% missing_AOIs$ID &
        Session %in% missing_AOIs$Session &
        Stimulus %in% missing_AOIs$Stimulus &
        StimOrder %in% missing_AOIs$StimOrder) %>%
    arrange(ID, Session, Stimulus, StimOrder, AOI)

# Problematic cases already present in raw data:
# multiple rows present for single AOIs (i.e. mouth), gets collapsed
# Probably also present in original analysis

# collapse even further, forming 5 groups
df_AOIgroups <- df_collapsed %>%
    mutate(AOI = case_when(
        AOI %in% c("Eyes", "brow") ~ "eye_brow",
        AOI %in% c("nose", "mouth", "jaw") ~ "nose_mouth_jaw",
        AOI %in% c("forehead", "Cheeks") ~ "forehead_cheek",
        AOI %in% c("White Space", "hair") ~ "whitespace_hair",
        TRUE ~ AOI
    )) %>%
    group_by(ID, Stimulus, StimOrder, AOI, Drug, AttrLevel, Gaze2, FaceGender, Imagelist, Session) %>%
    summarise(
        FixCount = sum(FixCount, na.rm = TRUE),
        FixTime = sum(FixTime, na.rm = TRUE),
        FixTimePerc = sum(FixTimePerc, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(ID, Session, StimOrder)

# Create Fix# (only fixations to face)
df_fix_n <- df_AOIgroups %>%
    mutate(AOI = case_when(
        AOI %in% c("eye_brow", "nose_mouth_jaw", "forehead_cheek") ~ "face",
        TRUE ~ AOI
    )) %>%
    group_by(ID, Stimulus, StimOrder, AOI, Drug, AttrLevel, Gaze2, FaceGender, Imagelist, Session) %>%
    summarise(
        FixCount = sum(FixCount, na.rm = TRUE),
        FixTime = sum(FixTime, na.rm = TRUE),
        FixTimePerc = sum(FixTimePerc, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    arrange(ID, Session, StimOrder)

df_fix_n %>% filter(AOI == "face", FaceGender == "F" & ID %in% c(101:130))
# 1780 rows, roughly the df in the published analysis (1729)

# save to csv
write_csv(df_fix_n, here("data", "analyses", "df_fix_n.csv"))

# Create fix_time_perc
df_fix_time_perc <- df_AOIgroups %>%
    group_by(ID, Session, Stimulus, StimOrder) %>%
    mutate(TotalFixTime = sum(FixTime, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(FixTimePerc = (FixTime / TotalFixTime) * 100)

df_fix_time_perc %>% filter(AOI != "whitespace_hair", FaceGender == "M", ID %in% c(101:130))
# 5340 rows, roughly the df in the published analysis (5279)

# save to csv
write_csv(df_fix_time_perc, here("data", "analyses", "df_fix_time_perc.csv"))
