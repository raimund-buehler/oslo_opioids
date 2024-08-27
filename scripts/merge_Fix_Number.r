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

session_etc %>%
    group_by(Drug) %>%
    summarise(n())

all_drugs %>%
    group_by(Drug) %>%
    summarise(n())

results_list <- list()

subjects <- unique(all_drugs$Subject)
drug_alldrugs <- "C"
drug_session <- 3

for (sub in subjects) {
    stims <- all_drugs %>%
        filter(Subject == sub, drug == drug_alldrugs) %>%
        select(Stimulus, Trial) %>%
        distinct()

    stims_sess <- session_etc %>%
        filter(ID == sub, Drug == drug_session) %>%
        pull(Stimulus) %>%
        unique() %>%
        tolower()

    stims_in_sess <- stims$Stimulus %in% stims_sess

    results_list[[as.character(sub)]] <- stims %>%
        mutate(Subject = sub, In_Session = stims_in_sess)
}

# Combine all results into one dataframe
final_results <- do.call(rbind, results_list)

missing_n <- final_results %>%
    filter(In_Session == FALSE) %>%
    group_by(Subject) %>%
    summarise(n())

stims <- all_drugs %>%
    filter(Subject == 101, drug == "A") %>%
    select(Stimulus, Trial) %>%
    distinct()

stims_sess <- session_etc %>%
    filter(ID == 101, Drug == 1) %>%
    pull(Stimulus) %>%
    unique() %>%
    tolower()

stims$Stimulus %in% stims_sess

stims$Stimulus
stims_sess
