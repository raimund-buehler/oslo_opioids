library(tidyverse)
library(here)
library(readxl)
library(writexl)

merged <- read_csv(here("data", "6_merged_Drug_StimOrder.csv"))

imagelist_dict <- readRDS("data/imagelist_dict.rds")

# Step 2: Function to find the best matching Imagelist for a given set of stimuli
find_best_imagelist <- function(stimuli, imagelist_dict) {
    best_match <- NULL
    best_overlap <- 0

    for (imlist in names(imagelist_dict)) {
        overlap <- length(intersect(stimuli, imagelist_dict[[imlist]]))
        if (overlap > best_overlap) {
            best_overlap <- overlap
            best_match <- imlist
        }
    }

    return(best_match)
}

# Step 3: Apply the function to each session to assign the best matching Imagelist
# Example of weighted assignment logic
session_imlist_assignment <- merged %>%
    group_by(Subject, drug) %>%
    summarize(Stimuli = list(Stimulus)) %>%
    mutate(Imagelist = sapply(Stimuli, function(stims) {
        best_match <- NULL
        best_score <- 0
        for (imlist in names(imagelist_dict)) {
            matches <- length(intersect(stims, imagelist_dict[[imlist]]))
            unique_A <- length(setdiff(imagelist_dict[[imlist]], stims))
            score <- matches - 0.5 * unique_A # Adjust weight as needed
            if (score > best_score) {
                best_score <- score
                best_match <- imlist
            }
        }
        return(best_match)
    })) %>%
    unnest(cols = c(Stimuli))


# View the result
print(session_imlist_assignment)

# get sessions etc from 3_inconsistencies_merge_prep.r, hedda clean

session_path <- here("data", "VisualLiking2 (hedda)_Guro_edit_2024 SESSION STIMLIST DRUG INFO.xlsx")
sheet <- "clean"

session_etc <- read_xlsx(session_path, sheet = sheet) %>%
    rename_with(~"Stimulus", matches("Stim|Stimulus")) %>%
    rename_with(~"ID", matches("Subject|ID")) %>%
    rename_with(~"Gaze2", matches("Gaze2|G2ze2|GazeDirect2")) %>%
    rename_with(~"AttrLevel", matches("AttrLevel|DatabaseAttrLevel")) %>%
    rename_with(~"FaceGender", matches("Gender|FaceGender")) %>%
    mutate(Stimulus = tolower(Stimulus)) %>%
    arrange(Drug, ID)

session_etc <- session_etc %>%
    mutate(Drug = recode(Drug,
        `1` = "morphine",
        `2` = "placebo",
        `3` = "naltrexone"
    ))

all_lists <- session_etc %>%
    distinct(ID, Drug, Imagelist) %>%
    arrange(ID, Drug)

assigned <- session_imlist_assignment %>%
    distinct(Subject, drug, Imagelist) %>%
    arrange(Subject, drug)

# List Number Matches, checked also with VL_1.xlsx, which only has number info


merged <- left_join(merged, assigned, by = c("Subject" = "Subject", "drug" = "drug"))

# recode Imagelist

merged <- merged %>%
    mutate(Imagelist = recode(Imagelist,
        Picture_L_1A = 1,
        Picture_L_2A = 2,
        Picture_L_3A = 3,
        Picture_L_4A = 4,
        Picture_L_5A = 5,
        Picture_L_6A = 6,
    ))

merged %>% distinct(Imagelist)

# Save the merged data with Imagelist assignment

write_csv(merged, here("data", "7_merged_Imagelist.csv"))

##### CECK OVERLAP/IDENTICAL LISTS (Not relevant)

imagelist_df <- enframe(imagelist_dict, name = "Imagelist", value = "Stimuli")

# Step 1: Separate the Imagelist into Number and Letter parts
imagelist_df <- imagelist_df %>%
    mutate(
        NumberPart = gsub("[^0-9]", "", Imagelist),
        LetterPart = gsub("[0-9]", "", Imagelist)
    )

# Step 2: Create a function to check if two sets of stimuli are identical
check_identical_stimuli <- function(df) {
    if (nrow(df) == 2) {
        return(identical(sort(df$Stimuli[[1]]), sort(df$Stimuli[[2]])))
    } else {
        return(FALSE)
    }
}

# Step 3: Compare A and B sets for each number
comparison_results <- imagelist_df %>%
    group_by(NumberPart) %>%
    filter(n() == 2) %>%
    summarize(Identical = check_identical_stimuli(cur_data())) %>%
    ungroup()

# Step 4: Filter out the pairs where A and B are identical
identical_pairs <- comparison_results %>%
    filter(Identical == TRUE)

# View the result
print(identical_pairs)

# Check the overlap between A and B for each number
overlap_results <- imagelist_df %>%
    group_by(NumberPart) %>%
    filter(n() == 2) %>%
    summarize(
        Overlap = length(intersect(Stimuli[[1]], Stimuli[[2]])),
        Total_A = length(Stimuli[[1]]),
        Total_B = length(Stimuli[[2]])
    ) %>%
    ungroup()

# View overlap results
print(overlap_results)
