library(tidyverse)
library(here)
library(readxl)
library(writexl)

merged <- read_csv(here("data", "merged_GazeDir_Gender_Attr.csv"))

merged %>%
    group_by(drug) %>%
    summarise(n = n())

# Recode the 'drug' column
merged <- merged %>%
    mutate(drug = recode(drug,
        "A" = "morphine",
        "B" = "placebo",
        "C" = "naltrexone"
    ))


# read last three numbers from Trial column
merged <- merged %>%
    mutate(Trial = as.numeric(str_sub(Trial, -3, -1))) %>%
    arrange(Subject, drug)


# Create StimOrder column by using consecutive Trial numbers per subject and drug combination

merged <- merged %>%
    group_by(Subject, drug) %>%
    mutate(StimOrder = dense_rank(Trial)) %>%
    ungroup()

max_stim <- merged %>%
    group_by(Subject, drug) %>%
    summarize(MaxStimOrder = max(StimOrder, na.rm = TRUE)) %>%
    ungroup()

# save the merged dataframe
write_csv(merged, here("data", "6_merged_Drug_StimOrder.csv"))
