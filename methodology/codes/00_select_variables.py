import pandas as pd
# view non-truncated dataframe
pd.set_option('display.max_colwidth', -1)
pd.set_option('display.max_columns', 30)
import numpy as np
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import available_vars

datafile = "data/full_data.csv"
data = pd.read_csv(datafile)
print(data.shape)
nrows = data.shape[0]
data.columns = map(str.lower, data.columns)

# keep variables from data dictionary - mostly created in cleaning codes
data_dictionary = pd.read_csv('data/data_dictionary.csv')
data_dictionary.columns = map(str.lower, data_dictionary.columns)
keep_cols = data_dictionary['column_name']
keep_cols = [col for col in keep_cols if 'sdoh_score' not in col]
data['med_2br_rent_per_med_inc'] = data['rent_twobed2015']/(data['median_income']/12)

print(keep_cols)
print(len(keep_cols))
data = data[keep_cols]
print(data.shape)

# are there any variables that don't have a ton of data? drop them if low coverage
count = 0
for var in data.columns.values:
    coverage = data[pd.notnull(data[var])].shape[0]/nrows
    # TODO: this is an arbitrary cutoff
    if coverage < 0.8:
        print(f"Dropping {var} due to low coverage ({round(coverage,2)*100}%)")
        data.drop([var], axis = 1, inplace = True)
        count += 1
print("# columns dropped due to low coverage: " + str(count))

data.to_csv('data/full_data_relative.csv', index = False)