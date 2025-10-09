import pandas as pd

# Load the dataset
file_path = 'data/ASQ_genetics_lookup/unified_dataset.csv'
df = pd.read_csv(file_path)

# Step 1: Filter IDs with exactly three digits
df = df[df['ID'].astype(str).str.match(r'^\d{3}$')]

# Step 2: Consolidate ASQ values
asq_columns = [col for col in df.columns if "ASQ" in col]
df['ASQ'] = df[asq_columns].mean(axis=1, skipna=True)

# Step 3: Normalize Genetics values
genetics_columns = [col for col in df.columns if "Genetics" in col]

def normalize_genetics(value):
    """Normalize genetics values to 'A' or 'G'."""
    if pd.isna(value):
        return None
    if value == 1 or value == "A":
        return "A"
    if value == 2 or value == "G":
        return "G"
    return None

# Apply normalization to all Genetics columns
for col in genetics_columns:
    df[col] = df[col].apply(normalize_genetics)

# Step 4: Collapse rows for each participant
def consolidate_participant_data(group):
    """Aggregate data for each participant."""
    asq_value = group['ASQ'].mean(skipna=True)  # Average ASQ
    genetics_value = group[genetics_columns].mode().iloc[0, 0] if not group[genetics_columns].mode().empty else None  # Most frequent Genetics
    return pd.Series({'ASQ': asq_value, 'Genetics': genetics_value})

# Group by participant ID and aggregate
collapsed_df = df.groupby('ID').apply(consolidate_participant_data).reset_index()

# Step 5: Ensure all participants are included
# No rows are dropped, even if ASQ or Genetics are NaN

# Save the final dataset
output_path = 'data/ASQ_genetics_lookup/cleaned_dataset.csv'
collapsed_df.to_csv(output_path, index=False)
print(f"Final dataset saved to {output_path}")
