library(tidyverse)
library(readxl)
library(writexl)

# 8AOIs
DrugA <- read_xlsx("data/DrugA.xlsx", sheet = "8AOIs")
DrugB <- read_xlsx("data/DrugB.xlsx", sheet = "AOIs")
DrugC <- read_xlsx("data/DrugC.xlsx", sheet = "Sheet3")

# All AOIs
DrugA <- read_xlsx("data/DrugA.xlsx", sheet = "raw")
DrugB <- read_xlsx("data/DrugB.xlsx", sheet = "raw")
DrugC <- read_xlsx("data/DrugC.xlsx", sheet = "raw")

DrugA$drug <- "A"
DrugB$drug <- "B"
DrugC$drug <- "C"

cols_A <- colnames(DrugA)
cols_B <- colnames(DrugB)
cols_C <- colnames(DrugC)

# Check which columns are not the same
unique_to_A <- setdiff(cols_A, union(cols_B, cols_C))
unique_to_B <- setdiff(cols_B, union(cols_A, cols_C))
unique_to_C <- setdiff(cols_C, union(cols_A, cols_B))

# Rename all unique columns to the same name (perc_AOI)
DrugA <- DrugA %>% rename(perc_AOI = one_of(unique_to_A))
DrugB <- DrugB %>% rename(perc_AOI = one_of(unique_to_B))
DrugC <- DrugC %>% rename(perc_AOI = one_of(unique_to_C))

DrugB <- DrugB %>% select(-any_of(unique_to_B))

# harmonize coltypes
# Function to coerce columns with the same name to the most common type
harmonize_types <- function(df_list) {
    # Get all column names across dataframes
    all_columns <- Reduce(union, lapply(df_list, colnames))

    # Convert each dataframe to have the same type for each column
    df_list <- lapply(df_list, function(df) {
        for (col in all_columns) {
            if (col %in% colnames(df)) {
                # Get the column type in other dataframes
                col_types <- sapply(df_list, function(x) if (col %in% colnames(x)) class(x[[col]]) else NA)
                # Choose the most common type
                common_type <- names(sort(table(col_types), decreasing = TRUE))[1]

                # Convert the column to the most common type
                df[[col]] <- switch(common_type,
                    "character" = as.character(df[[col]]),
                    "numeric" = as.numeric(df[[col]]),
                    "integer" = as.integer(df[[col]]),
                    "logical" = as.logical(df[[col]]),
                    df[[col]]
                ) # Default: no conversion
            }
        }
        return(df)
    })

    return(df_list)
}

# Apply the harmonization function to your dataframes
dfs <- harmonize_types(list(DrugA, DrugB, DrugC))

# Row-bind the harmonized dataframes
all_data <- bind_rows(dfs)

all_data <- all_data %>% arrange(drug, Subject, Trial)

# Save 8AOIs
write_xlsx(all_data, "data/AllDrugs_8AOIs.xlsx")

# Save All AOIs
write_xlsx(all_data, "data/AllDrugs_AllAOIs.xlsx")
