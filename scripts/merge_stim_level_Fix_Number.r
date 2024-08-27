library(tidyverse)
library(here)
library(readxl)
library(writexl)

drug_path <- here("data", "AllDrugs_8AOIs.xlsx")
session_path <- here("data", "Fix_Number.xlsx")

all_drugs <- read_xlsx(drug_path)

session_etc <- read_xlsx(session_path, sheet = "all") %>%
    rename(ID = "Subject") %>%
    arrange(Drug, ID)

# unique Stimulus in session_etc
sessions_stims <- session_etc %>%
    mutate(Stim = tolower(Stim)) %>%
    distinct(Stim) %>%
    pull(Stim)

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
    mutate(Stim = tolower(Stim)) %>%
    distinct(Stim, .keep_all = TRUE)

# TODO: Filter out missing stims from all_drugs
# create df with columns AttrLevel, G2ze, FaceGender, ImageList
# compare with ImList column from Fix_Number.xlsx, sheet: all
# StimOrder = TrialNumber? Check this. Get Stimorder for first 30 subjects from Fix_Number.xlsx, sheet: all
# refactor Drug column
# get session info from hedda, sheet: raw_complete
# Use the drug data with 10 AOIs, filter irrelevant AOIs
