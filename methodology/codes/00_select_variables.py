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

# TODO: could also use % race variables
# TODO: move this to the data processing pipeline
race_cols = available_vars(data, 'race')
print(race_cols)
drop_race_cols = ["race_estimate_total", "race_estimate_total_two_or_more_races_two_races_including_some_other_race",
"race_estimate_total_two_or_more_races_two_races_excluding_some_other_race_and_three_or_more_races"]
race_cols = np.setdiff1d(race_cols, drop_race_cols)
data[race_cols] = data[race_cols].apply(lambda x: x/data['race_estimate_total'])

# keep variables from data dictionary
data_dictionary = pd.read_csv('data/data_dictionary.csv')
keep_cols = data_dictionary['column_name']
keep_cols = [col for col in keep_cols if 'sdoh_score' not in col]

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

# # How are all of the demographic variables correlated?
# print(dem_vars)
# print(data[dem_vars].corr())
# # if no problematic correlations, flag all of these as demographic variables, and the rest as 0's
# # rural and some races have higher correlations, as does mortality and income, but going to keep them all in for now

# # # TODO: more variables could be flagged as sdoh_raw than just the ones that go into the aggregate sdoh scores, if we want

# def data_type(c):
#     if c in ['FIPS','County','State']:
#         return 'ID'
#     elif c in [f'sdoh_score{i}' for i in range(1,7)]:
#         return 'aggregate'
#     elif data[c].dtypes in ['float64','int64']:
#         if all(data[c] <= 1) or (('%' in c and '95%' not in c) or (c.count("%") > 1) or ('pct' in c)):
#             return 'percentage'
#         elif ('rate' in c.lower()) or ('ratio' in c.lower()):
#             return 'rate'
#         else:
#             return 'continuous'
#     else:
#         return 'unknown'

data.to_csv('data/full_data_relative.csv')