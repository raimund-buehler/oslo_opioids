import pandas as pd

# Load the dataset
df_fix_perc_asq_gen = pd.read_csv('data/analyses/fix_perc_ASQ_GEN.csv')

# Filter for rows where ID is 246
filtered_df = df_fix_perc_asq_gen[df_fix_perc_asq_gen['ID'] != 246]

# Count the number of rows for each ID
row_counts = filtered_df.groupby('ID').size().reset_index(name='RowCount')

# Print IDs where RowCount is not 480
not_480 = row_counts[row_counts['RowCount'] != 480]
print("IDs with RowCount not equal to 480:\n", not_480)

# Count distinct participants missing ASQ values
missing_asq_count = filtered_df[filtered_df['ASQ'].isna()]['ID'].nunique()
print(f"Number of distinct participants missing ASQ values: {missing_asq_count}")
# In percentage
missing_asq_perc = missing_asq_count / filtered_df['ID'].nunique() * 100
print(f"Percentage of distinct participants missing ASQ values: {missing_asq_perc:.2f}%")

missing_gen_count = filtered_df[filtered_df['Genetics'].isna()]['ID'].nunique()
print(f"Number of distinct participants missing genetics values: {missing_gen_count}")
# In percentage
missing_gen_perc = missing_gen_count / filtered_df['ID'].nunique() * 100
print(f"Percentage of distinct participants missing genetics values: {missing_gen_perc:.2f}%")

# Overwrite the original file with the filtered DataFrame
filtered_df.to_csv('data/analyses/fix_perc_ASQ_GEN.csv', index=False)
print("Filtered DataFrame saved back to 'data/analyses/fix_perc_ASQ_GEN.csv'")
