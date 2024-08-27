library(tidyverse)
library(readxl)
library(writexl)

all_drugs <- read_xlsx("data/AllDrugs_8AOIs.xlsx")

session_etc <- read_xlsx("data/VisualLiking_Guro_edit_march2024.xlsx") %>% arrange(Drug, ID)

session_etc %>%
    group_by(Drug, ID) %>%
    summarise(n())

all_drugs %>%
    group_by(Drug, Subject) %>%
    summarise(n())

results_list <- list()

subjects <- unique(all_drugs$Subject)
drug_alldrugs <- "A"
drug_session <- "morphine"

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

missing_n_march2024 <- final_results %>%
    filter(In_Session == FALSE) %>%
    group_by(Subject) %>%
    summarise(n())

stims_drugA <- all_drugs %>%
    filter(Subject == 239, drug == "A") %>%
    select(Stimulus, Trial) %>%
    distinct()

stims_drugA_sess <- session_etc %>%
    filter(ID == 239, Drug == "morphine") %>%
    pull(Stim) %>%
    unique() %>%
    tolower()

stims_drugA$Stimulus %in% stims_drugA_sess

stims_drugA$Stimulus
stims_drugA_sess
