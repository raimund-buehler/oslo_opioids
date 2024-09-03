library(tidyverse)
library(here)
library(readxl)
library(writexl)



#### FILES####
# Fix_Number
session_path <- here("data", "Fix_Number.xlsx")
sheet <- "all"

# Hedda_clean
session_path <- here("data", "VisualLiking2 (hedda)_Guro_edit_2024 SESSION STIMLIST DRUG INFO.xlsx")
sheet <- "clean"
sheet <- "raw_complete"

# March2024
session_path <- here("data", "VisualLiking_Guro_edit_march2024.xlsx")
sheet <- "rawdata"

# VisualLiking_complete
session_path <- here("data", "VisualLiking_complete.xlsx")
sheet <- "raw_complete"

# VL_1
session_path <- here("data", "VL_1.xlsx")
sheet <- "VL_1"

# DRUG FILE
# 8AOIs
drug_path <- here("data", "AllDrugs_8AOIs.xlsx")
# All AOIs
drug_path <- here("data", "AllDrugs_AllAOIs.xlsx")

all_drugs <- read_xlsx(drug_path)

session_etc <- read_xlsx(session_path, sheet = sheet) %>%
    rename_with(~"Stimulus", matches("Stim|Stimulus")) %>%
    rename_with(~"ID", matches("Subject|ID")) %>%
    rename_with(~"Gaze2", matches("Gaze2|G2ze2|GazeDirect2")) %>%
    rename_with(~"AttrLevel", matches("AttrLevel|DatabaseAttrLevel")) %>%
    rename_with(~"FaceGender", matches("Gender|FaceGender")) %>%
    mutate(Stimulus = tolower(Stimulus)) %>%
    arrange(Drug, ID)

# unique Stimulus in session_etc
sessions_stims <- session_etc %>%
    distinct(Stimulus) %>%
    pull(Stimulus)

all_drugs_stims <- all_drugs %>%
    distinct(Stimulus) %>%
    pull(Stimulus)


missing_stims <- setdiff(all_drugs_stims, sessions_stims)

# show trials in all_drugs for missing stims
missing_stims_trials <- all_drugs %>%
    filter(Stimulus %in% missing_stims) %>%
    distinct(Subject, Stimulus, Trial) %>%
    arrange(Subject) %>%
    select(Subject, Stimulus, Trial)

session_etc %>%
    mutate(Stimulus = tolower(Stimulus)) %>%
    distinct(Stimulus, .keep_all = TRUE)

# filter trials that are not Trial 002 or Trial 001
missing_n_part <- missing_stims_trials %>%
    # filter(Trial != "Trial002") %>%
    group_by(Subject) %>%
    summarise(n())

# create stimlevel df with columns AttrLevel, GazeDir, FaceGender

single_stims <- session_etc %>%
    select(Stimulus, AttrLevel, Gaze2, FaceGender) %>%
    distinct() %>%
    arrange(Stimulus)

# Inconsistencies for all files, different FaceGender and AttrLevel
# Which one inconsistent? Check for unique values

inconsistent_stims <- single_stims %>%
    group_by(Stimulus) %>%
    summarise(n = n()) %>%
    filter(n > 1)

inconsistent_stims <- single_stims %>%
    filter(Stimulus %in% inconsistent_stims$Stimulus) %>%
    arrange(Stimulus)

# 18 in most files

write_csv(single_stims, here("data", "single_stims.csv"))



# Session and ImageList (from hedda clean)
session_etc <- session_etc %>% select(ID, Session, Drug, Stimulus, Imagelist)

write_csv(session_etc %>% select(Stimulus, Imagelist), here("data", "Stim_ImList.csv"))

stimlist_df <- session_etc %>% select(Stimulus, Imagelist)

imagelist_dict <- stimlist_df %>%
    group_by(Imagelist) %>%
    summarize(Stimuli = list(Stimulus)) %>%
    deframe()

print(imagelist_dict)

saveRDS(imagelist_dict, "data/imagelist_dict.rds")
