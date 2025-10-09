import os
import pandas as pd
from fuzzywuzzy import fuzz
import pyreadstat

# Define keywords for fuzzy matching
ASQ_KEYWORDS = ["ASQ", "autism spectrum", "screening"]
GENETIC_KEYWORDS = ["gene", "genetic", "DNA"]
ID_KEYWORDS = ["ID", "participant", "subject"]

def fuzzy_match(column_name, keywords):
    """Check if a column name matches any keyword based on fuzzy logic."""
    for keyword in keywords:
        if fuzz.partial_ratio(column_name.lower(), keyword.lower()) > 80:
            return True
    return False

def read_file(filepath):
    """Read a file and return a DataFrame."""
    ext = os.path.splitext(filepath)[-1].lower()
    try:
        print(f"Attempting to read file: {filepath}")  # Debugging
        if ext == ".csv":
            return pd.read_csv(filepath), ext
        elif ext == ".xlsx":
            return pd.read_excel(filepath, engine="openpyxl"), ext
        elif ext == ".xls":
            return pd.read_excel(filepath, engine="xlrd"), ext
        elif ext == ".sav":
            df, meta = pyreadstat.read_sav(filepath)
            return df, ext
        else:
            print(f"Unsupported file extension: {ext}")  # Debugging
    except Exception as e:
        print(f"Error reading {filepath}: {e}")  # Debugging
    return None, None

def process_folder(folder_path):
    """Crawl the folder, extract potential ASQ and genetic columns."""
    lookup_table = []

    for root, _, files in os.walk(folder_path):
        for file in files:
            filepath = os.path.join(root, file)
            df, ext = read_file(filepath)
            if df is None:
                print(f"Skipping file: {filepath}")  # Debugging
                continue  # Skip files that couldn't be read

            for column in df.columns:
                is_asq = fuzzy_match(column, ASQ_KEYWORDS)
                is_genetic = fuzzy_match(column, GENETIC_KEYWORDS)
                is_id = fuzzy_match(column, ID_KEYWORDS)

                if is_asq or is_genetic or is_id:
                    print(f"Matched column: {column} in file: {filepath}")  # Debugging
                    lookup_table.append({
                        "file": filepath,
                        "column_name": column,
                        "is_asq": is_asq,
                        "is_genetic": is_genetic,
                        "is_id": is_id
                    })
    return pd.DataFrame(lookup_table)

def harmonize_columns(lookup_df, folder_path):
    """Create harmonized columns for ASQ and genetics."""
    harmonized_data = []

    for _, row in lookup_df.iterrows():
        filepath = row["file"]
        column_name = row["column_name"]
        df, _ = read_file(filepath)
        if df is not None:
            if row["is_id"]:
                id_column = column_name
            elif row["is_asq"]:
                asq_column = column_name
            elif row["is_genetic"]:
                genetic_column = column_name

            try:
                harmonized_data.append(df[[id_column, asq_column, genetic_column]].dropna())
            except KeyError as e:
                print(f"Missing columns in file {filepath}: {e}")  # Debugging

    # Combine all datasets into one harmonized dataset
    if harmonized_data:
        final_df = pd.concat(harmonized_data, ignore_index=True)
        final_df.columns = ["participant_id", "asq", "genetics"]
        return final_df
    else:
        print("No harmonized data found.")  # Debugging
        return pd.DataFrame(columns=["participant_id", "asq", "genetics"])

# Define folder path
folder_path = "data"

# Step 1: Create a lookup table
print("Processing folder to create lookup table...")  # Debugging
lookup_df = process_folder(folder_path)
lookup_df.to_csv("data/ASQ_genetics_lookup/lookup_table.csv", index=False)
print("Lookup table saved to lookup_table.csv")  # Debugging
