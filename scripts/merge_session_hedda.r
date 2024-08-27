library(tidyverse)
library(readxl)
library(writexl)

all_drugs <- read_xlsx("data/AllDrugs_8AOIs.xlsx")

session_etc <- read_xlsx("data/VisualLiking2 (hedda)_Guro_edit_2024 SESSION STIMLIST DRUG INFO.xlsx") %>% arrange(Drug, Subject)

session_etc$Drug <- toupper(session_etc$Drug)

results_list <- list()

subjects <- unique(all_drugs$Subject)

for (sub in subjects) {
    stims_drugA <- all_drugs %>%
        filter(Subject == sub, drug == "A") %>%
        select(Stimulus, Trial) %>%
        distinct()

    stims_drugA_sess <- session_etc %>%
        filter(Subject == sub, Drug == "A") %>%
        pull(Stim) %>%
        unique() %>%
        tolower()

    stims_in_sess <- stims_drugA$Stimulus %in% stims_drugA_sess

    results_list[[as.character(sub)]] <- stims_drugA %>%
        mutate(Subject = sub, In_Session = stims_in_sess)
}

# Combine all results into one dataframe
final_results <- do.call(rbind, results_list)

missing_n_hedda <- final_results %>%
    filter(In_Session == FALSE) %>%
    group_by(Subject) %>%
    summarise(n())

stims_drugA <- all_drugs %>%
    filter(Subject == 231, drug == "A") %>%
    select(Stimulus, Trial) %>%
    distinct()

stims_drugA_sess <- session_etc %>%
    filter(Subject == 231, Drug == "A") %>%
    pull(Stim) %>%
    unique() %>%
    tolower()

stims_drugA$Stimulus %in% stims_drugA_sess

stims_drugA$Stimulus
stims_drugA_sess
