library(tidyverse)
library(here)
library(readxl)
library(writexl)

# VL_1
session_path <- here("data", "VL_1.xlsx")
sheet <- "VL_1"

merged <- read_csv(here("data", "7_merged_Imagelist.csv")) %>% rename(ID = Subject, Drug = drug)

session_data <- read_xlsx(session_path, sheet = sheet) %>%
    rename_with(~"Stimulus", matches("Stim|Stimulus")) %>%
    rename_with(~"ID", matches("Subject|ID")) %>%
    # rename_with(~"Gaze2", matches("Gaze2|G2ze2|GazeDirect2")) %>%
    # rename_with(~"AttrLevel", matches("AttrLevel|DatabaseAttrLevel")) %>%
    # rename_with(~"FaceGender", matches("Gender|FaceGender")) %>%
    mutate(Stimulus = tolower(Stimulus)) %>%
    arrange(ID, Drug) %>%
    select(ID, Session, Drug) %>%
    distinct()

n_merged <- merged %>%
    select(ID, Drug) %>%
    distinct() %>%
    group_by(ID) %>%
    summarise(n())

n <- session_data %>%
    group_by(ID) %>%
    summarise(n_sessions = n())

session_data <- session_data %>%
    mutate(Drug = recode(Drug,
        `1` = "morphine",
        `2` = "placebo",
        `3` = "naltrexone"
    ))

# merge by ID and Drug
merged <- merged %>%
    left_join(session_data, by = c("ID", "Drug"))

sessions_merged <- merged %>%
    select(ID, Session, Drug) %>%
    distinct() %>%
    arrange(ID, Drug, Session)

session_data <- session_data %>% arrange(ID, Drug, Session)

# consistent, missing session 2 for 243 and session 1 for 246,
# replacing manually

merged <- merged %>%
    mutate(Session = ifelse(ID == 243 & is.na(Session), 2,
        ifelse(ID == 246 & is.na(Session), 1, Session)
    ))

# other sessions for 243 and 246 not in raw data

# save the merged data
write_csv(merged, here("data", "8_merged_session.csv"))
