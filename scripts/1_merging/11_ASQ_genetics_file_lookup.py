import pandas as pd
from fuzzywuzzy import fuzz
import warnings
import os

# Suppress warnings from openpyxl
warnings.filterwarnings("ignore", category=UserWarning, module="openpyxl")

# Define keywords for fuzzy matching
ASQ_KEYWORDS = ["ASQ", "autism spectrum", "screening"]
GENETIC_KEYWORDS = ["gene", "genetic", "DNA"]
ID_KEYWORDS = ["ID", "participant", "subject"]

# Define sheets to skip for specific files
sheets_to_skip = {
    "data/VisualLiking_complete.xlsx": ["New attr_rating", "pivot", "Ark4"]
}

def strict_id_match(column_name):
    """Strictly match column names to ID keywords."""
    # Exact match
    if column_name.strip().lower() in [kw.lower() for kw in ID_KEYWORDS]:
        return True
    # High fuzzy matching threshold
    for keyword in ID_KEYWORDS:
        if fuzz.partial_ratio(column_name.lower(), keyword.lower()) > 90:
            return True
    return False

def fuzzy_match(column_name, keywords):
    """Check if a column name matches any keyword based on fuzzy logic."""
    for keyword in keywords:
        if fuzz.partial_ratio(column_name.lower(), keyword.lower()) > 80:
            return True
    return False

def extract_data_from_file(filepath):
    """Extract ID, ASQ, and Genetics columns from an Excel file."""
    all_data = []
    try:
        print(f"Processing file: {filepath}")
        excel_file = pd.ExcelFile(filepath)
        for sheet_name in excel_file.sheet_names:
            # Skip sheets as specified
            if filepath in sheets_to_skip and sheet_name in sheets_to_skip[filepath]:
                print(f"Skipping sheet: {sheet_name} in file: {filepath}")
                continue

            print(f"Processing sheet: {sheet_name}")
            sheet_data = pd.read_excel(filepath, sheet_name=sheet_name)
            print(f"Columns in sheet {sheet_name} of file {filepath}: {sheet_data.columns.tolist()}")
            
            id_column = None
            asq_column = None
            genetic_column = None

            # Identify relevant columns
            for column in sheet_data.columns:
                if id_column is None and strict_id_match(column):
                    id_column = column
                elif fuzzy_match(column, ASQ_KEYWORDS) and asq_column is None:
                    asq_column = column
                elif fuzzy_match(column, GENETIC_KEYWORDS) and genetic_column is None:
                    genetic_column = column

            print(f"Matched ID column: {id_column}")
            print(f"Matched ASQ column: {asq_column}")
            print(f"Matched Genetics column: {genetic_column}")

            if id_column:
                sheet_data.rename(columns={id_column: "ID"}, inplace=True)  # Standardize ID column name
                temp_df = pd.DataFrame()
                temp_df["ID"] = sheet_data["ID"]
                if asq_column and asq_column in sheet_data.columns:
                    temp_df["ASQ"] = sheet_data[asq_column]
                if genetic_column and genetic_column in sheet_data.columns:
                    temp_df["Genetics"] = sheet_data[genetic_column]
                all_data.append(temp_df)
            else:
                print(f"No ID column found in {filepath} - {sheet_name}")

    except Exception as e:
        print(f"Error processing {filepath}: {e}")
    return pd.concat(all_data, ignore_index=True) if all_data else pd.DataFrame()


def create_unified_dataset(file_list):
    """Create a unified dataset of IDs and merge ASQ/Genetics columns."""
    combined_file = "data/ASQ_genetics_lookup/combined_data_temp.csv"
    merged_dataset = pd.DataFrame(columns=["ID"])
    
    for filepath in file_list:
        try:
            file_data = extract_data_from_file(filepath)
            
            if not file_data.empty:
                print(f"Processing file {filepath} with {file_data.shape[0]} rows.")
                
                # Dynamically select available columns for merging
                columns_to_merge = ["ID"]
                if "ASQ" in file_data.columns:
                    columns_to_merge.append("ASQ")
                if "Genetics" in file_data.columns:
                    columns_to_merge.append("Genetics")

                if os.path.exists(combined_file):
                    # Load existing merged data
                    merged_dataset = pd.read_csv(combined_file)
                merged_dataset = merged_dataset.merge(
                    file_data[columns_to_merge], on="ID", how="outer", suffixes=("", f"_{filepath}")
                )
                
                # Drop duplicates to avoid exponential growth
                merged_dataset.drop_duplicates(inplace=True)

                # Save intermediate result
                merged_dataset.to_csv(combined_file, index=False)
                print(f"Intermediate merged dataset saved after processing {filepath}. Dimensions: {merged_dataset.shape}")
            else:
                print(f"No relevant data found in file: {filepath}")
        except Exception as e:
            print(f"Error processing or merging data from file {filepath}: {e}")

    # Load final merged dataset
    if os.path.exists(combined_file):
        return pd.read_csv(combined_file)
    else:
        print("No combined data file found.")
        return pd.DataFrame()

# Specify the list of Excel files to process
file_list = [
    "data/VL_1.xlsx",
    "data/VisualLiking2 (hedda)_Guro_edit_2024 SESSION STIMLIST DRUG INFO.xlsx",
    "data/VisualLiking_complete.xlsx",
    "data/State&Trait-data.xlsx",
    "data/Gaze_ASQ.xlsx"
]

# Process the specified files and create a unified dataset
print("Creating unified dataset...")
try:
    unified_dataset = create_unified_dataset(file_list)
    # Save the unified dataset
    if not unified_dataset.empty:
        unified_dataset.to_csv("data/ASQ_genetics_lookup/unified_dataset.csv", index=False)
        print("Unified dataset saved to unified_dataset.csv")
    else:
        print("Unified dataset was not created due to missing relevant data.")
except Exception as e:
    print(f"An error occurred during processing: {e}")
