library(tidyverse)
library(here)
library(readxl)
library(writexl)

df <- read_csv(here("data", "8_merged_session.csv"))

# Number of Trials
n_trials <- df %>%
    group_by(ID, Session) %>%
    summarise(n = n())
# 11 AOIs * 40 trials = 440

n_trials %>%
    filter(n == 440) %>%
    nrow()
# 135 fine sessions
n_trials %>% filter(n != 440)
# 10 oddballs

# Number of Sessions
n_sessions <- df %>%
    group_by(ID) %>%
    summarise(n = n_distinct(Session))

n_sessions %>% filter(n != 3)
# 4 odd ones

# check in initial drug data
# ALL AOIs
drug_path <- here("data", "AllDrugs_AllAOIs.xlsx")
# 8 AOIs
drug_path <- here("data", "AllDrugs_8AOIs.xlsx")

all_drugs <- read_xlsx(drug_path)

n_sessions_original <- all_drugs %>%
    group_by(Subject) %>%
    summarise(n = n_distinct(drug))

n_sessions_original %>% filter(n != 3)
# same result (13 was filtered), raw data missing for subs above 230
# 114 likely also not present
