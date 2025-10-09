import pandas as pd

# Load the dataset
df_fix_perc_asq_gen = pd.read_csv('data/analyses/fix_perc_ASQ_GEN.csv')

# Filter for rows where StimOrder is >= 39
filtered_df = df_fix_perc_asq_gen[df_fix_perc_asq_gen['StimOrder'] <= 40]

# Count the number of rows for each ID
row_counts = filtered_df.groupby('ID').size().reset_index(name='RowCount')

# Print IDs where RowCount is not 480
not_480 = row_counts[row_counts['RowCount'] != 480]
print("IDs with RowCount not equal to 480:\n", not_480)

# Overwrite the original file with the filtered DataFrame
filtered_df.to_csv('data/analyses/fix_perc_ASQ_GEN.csv', index=False)
print("Filtered DataFrame saved back to 'data/analyses/fix_perc_ASQ_GEN.csv'")


