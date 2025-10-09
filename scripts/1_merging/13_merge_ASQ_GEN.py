import pandas as pd

# Load the datasets
cleaned_data_path = 'data/ASQ_genetics_lookup/cleaned_dataset.csv'
fix_time_data_path = 'data/analyses/df_fix_time_perc.csv'

cleaned_df = pd.read_csv(cleaned_data_path)
fix_time_df = pd.read_csv(fix_time_data_path)

# Merge the datasets by 'ID'
merged_df = pd.merge(fix_time_df, cleaned_df, on='ID', how='left')

# Save the merged dataset
output_path = 'data/analyses/fix_perc_ASQ_GEN.csv'
merged_df.to_csv(output_path, index=False)

print(f"Merged dataset saved to {output_path}")

# Count distinct participants missing ASQ values
missing_asq_count = cleaned_df[cleaned_df['ASQ'].isna()]['ID'].nunique()
print(f"Number of distinct participants missing ASQ values: {missing_asq_count}")

missing_gen_count = cleaned_df[cleaned_df['Genetics'].isna()]['ID'].nunique()
print(f"Number of distinct participants missing genetics values: {missing_gen_count}")