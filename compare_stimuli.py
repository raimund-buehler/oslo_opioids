import pandas as pd
from fuzzywuzzy import process

# Load your datasets
# Adjust the file paths as needed
df_fix_time_perc = pd.read_csv('data/analyses/df_fix_time_perc.csv')
raw_complete = pd.read_excel('data/VisualLiking2 (hedda)_Guro_edit_2024 SESSION STIMLIST DRUG INFO.xlsx', sheet_name='raw_complete')
clean = pd.read_excel('data/VisualLiking2 (hedda)_Guro_edit_2024 SESSION STIMLIST DRUG INFO.xlsx', sheet_name='clean')

# Helper function to fuzzy match column names
def fuzzy_find_column(df, target_columns):
    matched_columns = {}
    for target in target_columns:
        best_match, score = process.extractOne(target, df.columns)
        if score > 80:  # Adjust threshold if necessary
            matched_columns[target] = best_match
    return matched_columns

# Define the columns you want to compare
columns_to_match = ['ID', 'stimulus', 'session']

# Fuzzy match columns in each dataset
df_fix_cols = fuzzy_find_column(df_fix_time_perc, columns_to_match)
raw_cols = fuzzy_find_column(raw_complete, columns_to_match)
clean_cols = fuzzy_find_column(clean, columns_to_match)

# Extract relevant columns using matched names
df_fix_data = df_fix_time_perc[[df_fix_cols['ID'], df_fix_cols['stimulus'], df_fix_cols['session']]].drop_duplicates()
raw_data = raw_complete[[raw_cols['ID'], raw_cols['stimulus'], raw_cols['session']]].drop_duplicates()
clean_data = clean[[clean_cols['ID'], clean_cols['stimulus'], clean_cols['session']]].drop_duplicates()

# Rename columns for consistency
df_fix_data.columns = ['ID', 'stimulus', 'session']
raw_data.columns = ['ID', 'stimulus', 'session']
clean_data.columns = ['ID', 'stimulus', 'session']

# Normalize text to lowercase to discard case inconsistencies
df_fix_data['stimulus'] = df_fix_data['stimulus'].str.lower()
raw_data['stimulus'] = raw_data['stimulus'].str.lower()
clean_data['stimulus'] = clean_data['stimulus'].str.lower()

# Find mismatched stimuli
def find_mismatches(df1, df2, key=['ID', 'stimulus', 'session']):
    merged = pd.merge(df1, df2, on=key, how='outer', indicator=True)
    mismatches = merged[merged['_merge'] != 'both']
    return mismatches

mismatches_fix_vs_raw = find_mismatches(df_fix_data, raw_data)
mismatches_fix_vs_clean = find_mismatches(df_fix_data, clean_data)
mismatches_raw_vs_clean = find_mismatches(raw_data, clean_data)

# Print debugging information
print("Column mapping for df_fix_time_perc:", df_fix_cols)
print("Column mapping for raw_complete:", raw_cols)
print("Column mapping for clean:", clean_cols)
print("Mismatches Fix vs Raw:\n", mismatches_fix_vs_raw)
print("Mismatches Fix vs Clean:\n", mismatches_fix_vs_clean)
print("Mismatches Raw vs Clean:\n", mismatches_raw_vs_clean)

# Save mismatched stimuli to files
mismatches_fix_vs_raw.to_excel('mismatches_fix_vs_raw.xlsx', index=False)
mismatches_fix_vs_clean.to_excel('mismatches_fix_vs_clean.xlsx', index=False)
mismatches_raw_vs_clean.to_excel('mismatches_raw_vs_clean.xlsx', index=False)

print("Mismatches saved to 'mismatches_fix_vs_raw.xlsx', 'mismatches_fix_vs_clean.xlsx', and 'mismatches_raw_vs_clean.xlsx'")
