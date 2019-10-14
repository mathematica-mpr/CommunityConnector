import pandas as pd 
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import pull_population
import numpy as np

data = pd.read_csv('data/raw/dial_fac_data.csv')

mapping = pd.read_csv('data/raw/county_fips.csv')
mapping['county'] = mapping['CTYNAME'].str.replace(' County','').str.lower()
mapping = mapping[mapping.STNAME == "Colorado"]
mapping = mapping[['county','FIPS']].drop_duplicates()

print(data.shape)
data = pd.merge(data, mapping, on = 'county', how = 'left')

pop = pull_population()
print(pop)

data = pd.merge(data, pop, on = 'FIPS', how = 'left')
data['dial_facil_pp_rate'] = data['num_dial_facil']/data['population']
data.drop(['population','county','num_dial_facil'], axis = 1, inplace = True)

data['dial_fac_avg_mort'] = data['dial_fac_avg_mort']/100
data['dial_fac_avg_readm'] = data['dial_fac_avg_readm']/100

print(data.shape)
print(data)

# add to data dictionary
# TODO: use function from utilities
data_dict = pd.read_csv('data/data_dictionary.csv')
add_cols = data.columns.values
add_cols = [c for c in add_cols if c != "FIPS"]
# remove if they are already in it
rest_cols = pd.DataFrame(np.setdiff1d(data_dict['column_name'], add_cols))
rest_cols.columns = ["column_name"]
pre_rows = data_dict.shape[0]
data_dict = pd.merge(data_dict, rest_cols, on = "column_name")
print(f"Dropped {pre_rows - data_dict.shape[0]} existing rows")

# too low of coverage so no longer keeping these variables
# TODO make this into a user input tool?
# add_rows = pd.DataFrame({'column_name': add_cols,
# 'description': ['Medicare Dialysis Facilities Avg Rating','Medicare Dialysis Facilities Avg Stations',
#  'Medicare Dialysis Facilities Avg Mortality','Medicare Dialysis Facilities Avg Readmission','Medicare Dialysis Facilities per Person'],
# 'demographic': [0, 0, 0, 0, 0],
# 'sdoh_raw': [1, 1, 1, 1, 1],
# 'outcome': [0, 0, 0, 0, 0],
# 'sdoh_score': [0, 0, 0, 0, 0],
# 'data_type': ['continuous','continouous','percentage','percentage','rate'],
# 'used_sdoh_1': [0, 0, 0, 0, 0],
# 'used_sdoh_2': [0, 0, 0, 0, 0],
# 'used_sdoh_3': [0, 0, 0, 0, 0],
# 'used_sdoh_4': [0, 0, 0, 0, 0],
# 'used_sdoh_5': [0, 0, 0, 0, 0],
# 'used_sdoh_6': [1, 1, 1, 1, 1]})
# data_dict = data_dict.append(add_rows)

data_dict.to_csv('data/data_dictionary.csv')

data.to_csv('data/cleaned/02_SDoH/dial_fac_cleaned.csv', index = False)