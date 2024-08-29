library(tidyverse)
library(here)
library(readxl)
library(writexl)

drug_path <- here("data", "AllDrugs_AllAOIs.xlsx")

all_drugs <- read_xlsx(drug_path) %>% arrange(Subject, drug)

stim_path <- here("data", "cleaned_stims.csv")

# f051c, AttrLevel 3 is not correct
stims <- read_csv(stim_path)

# filter stims in all_drugs that are not in stims
missing_stims <- setdiff(all_drugs$Stimulus, stims$Stimulus)

all_drugs <- all_drugs %>%
    filter(!Stimulus %in% missing_stims)

merged <- left_join(all_drugs, stims, by = "Stimulus")

# How many trials per subject per drug?
trials_n <- merged %>%
    group_by(Subject, drug) %>%
    summarise(n = n())

inconsistent_trials <- trials_n %>% filter(n != 440)

inconsistent <- merged %>%
    semi_join(inconsistent_trials, by = c("Subject", "drug"))

inconsistent_first <- inconsistent %>%
    group_by(Subject, drug) %>%
    filter(Trial == first(Trial)) %>%
    distinct(Trial, .keep_all = TRUE) %>%
    ungroup()

# Filter Trial002 (invalid) for first 30 and Subject 13
merged <- merged %>%
    filter(
        Trial != "Trial002",
        Subject != 13
    )

# Some inconsistencies remain for later subs, 234, 238, 240, 244, not due to calibration Trials

# Save the merged dataframe
write_csv(merged, here("data", "merged_GazeDir_Gender_Attr.csv"))
