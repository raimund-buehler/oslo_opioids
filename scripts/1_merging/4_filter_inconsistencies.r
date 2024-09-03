# Load necessary libraries
library(dplyr)
library(tidyr)

# Load your data
df <- read.csv("data/single_stims.csv")

# Function to extract information from Stimulus name
extract_info <- function(stimulus) {
    match <- regmatches(stimulus, regexec("(f|m)(\\d{3})([clr])\\.jpg", stimulus))
    if (length(match[[1]]) > 1) {
        gender <- ifelse(match[[1]][2] == "f", "F", "M")
        gaze2 <- ifelse(match[[1]][4] == "c", "C", "A")
        return(c(gender, gaze2))
    } else {
        return(c(NA, NA))
    }
}

# Apply the extract_info function to each Stimulus
df <- df %>%
    rowwise() %>%
    mutate(Extracted = list(extract_info(Stimulus))) %>%
    unnest_wider(Extracted, names_sep = "_") %>%
    rename(FaceGender_extracted = Extracted_1, Gaze2_extracted = Extracted_2)

# Filter rows where the extracted information matches the existing columns
df_clean <- df %>%
    filter(
        (is.na(FaceGender) | FaceGender == FaceGender_extracted) &
            (is.na(Gaze2) | Gaze2 == Gaze2_extracted)
    ) %>%
    select(-starts_with("Extracted")) # Remove the temporary extracted columns

# View the cleaned dataframe
print(df_clean)

df_clean %>% filter(Stimulus %in% inconsistent_stims$Stimulus)
# one inconsistency remains: f051c.jpg, AttrLevel 3
df_clean <- df_clean %>%
    filter(!(Stimulus == "f051c.jpg" & AttrLevel == 3)) %>%
    select(1:4)

# Save the cleaned dataframe if needed
write.csv(df_clean, "data/cleaned_stims.csv", row.names = FALSE)
