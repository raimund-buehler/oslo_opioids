library(tidyverse)
library(readxl)
library(writexl)

all_drugs <- read_xlsx("data/AllDrugs_8AOIs.xlsx")

session_etc <- read_xlsx("data/VisualLiking2 (hedda)_Guro_edit_2024 SESSION STIMLIST DRUG INFO.xlsx", sheet = "clean") %>% arrange(Drug, ID)

session_etc %>%
    group_by(Drug) %>%
    summarise(n())

all_drugs %>%
    group_by(Drug, Subject) %>%
    summarise(n())

results_list <- list()

subjects <- unique(all_drugs$Subject)
drug_alldrugs <- "A"
drug_session <- 1

for (sub in subjects) {
    stims_drugA <- all_drugs %>%
        filter(Subject == sub, drug == drug_alldrugs) %>%
        select(Stimulus, Trial) %>%
        distinct()

    stims_drugA_sess <- session_etc %>%
        filter(ID == sub, Drug == drug_session) %>%
        pull(Stim) %>%
        unique() %>%
        tolower()

    stims_in_sess <- stims_drugA$Stimulus %in% stims_drugA_sess

    results_list[[as.character(sub)]] <- stims_drugA %>%
        mutate(Subject = sub, In_Session = stims_in_sess)
}

# Combine all results into one dataframe
final_results <- do.call(rbind, results_list)

missing_n_hedda_clean <- final_results %>%
    filter(In_Session == FALSE) %>%
    group_by(Subject) %>%
    summarise(n())

stims_drugA <- all_drugs %>%
    filter(Subject == 117, drug == "A") %>%
    select(Stimulus, Trial) %>%
    distinct()

stims_drugA_sess <- session_etc %>%
    filter(ID == 117, Drug == 1) %>%
    pull(Stim) %>%
    unique() %>%
    tolower()

stims_drugA$Stimulus %in% stims_drugA_sess

stims_drugA$Stimulus
stims_drugA_sess
