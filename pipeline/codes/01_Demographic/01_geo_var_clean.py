import pandas as pd
pd.set_option('display.max_colwidth', -1)
pd.set_option('display.max_columns', 30)
import os
import zipfile
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import pull_population, remove_from_dict
import numpy as np

raw_output = 'data/raw/'
output = 'data/cleaned/01_Demographic'

# unzip
with zipfile.ZipFile(os.path.join(raw_output, 'geo_var_data.zip'), 'r') as zip_ref:
    zip_ref.extractall(raw_output)

data = pd.read_excel(os.path.join(raw_output, "State County All Table 2017.xlsx"),
sheet_name = "State_county 2017")
# replace column names and drop first filler row
data.columns = data.loc[0]
data.drop([0], inplace = True)

# filter to Colorado county data
data = data[data['State'] == "CO"]
data = data[pd.notnull(data['State and County FIPS Code'])]

# keep only the columns we need
keep_cols = ['State and County FIPS Code','Beneficiaries with Part A and Part B','Average Age','Percent Female','Average HCC Score',
'Standardized Risk-Adjusted Per Capita Costs']
data = data[keep_cols]
data.columns = ['FIPS','Medicare_beneficiaries','Medicare_avg_age','Medicare_pct_female','Medicare_avg_hcc','Medicare_std_adj_cost_pp']
data['Medicare_pct_female'] = [pct[:-2] for pct in data['Medicare_pct_female']]

# normalize # of beneficiaries
pop = pull_population()
data = pd.merge(data, pop, on = "FIPS", how = 'left')
data = data.replace("*", -1, regex = False)
data['Pct_Medicare_beneficiaries'] = data['Medicare_beneficiaries'].astype(float)/data['population']

# TODO: not sure why I have to do this, but the Medicare_pct_female won't change to float without it
data.to_csv(os.path.join(raw_output, "geo_var_cleaned_pre.csv"), index = False)
data = pd.read_csv(os.path.join(raw_output, "geo_var_cleaned_pre.csv"))

data['Medicare_pct_female'] = data['Medicare_pct_female'].astype(float)/100
data.drop(['population','Medicare_beneficiaries'], axis = 1, inplace = True)

# replace negatives with NAs
data[data < 0] = np.nan
print(data.head())

results = remove_from_dict(data)
data_dict = results[0]
add_cols = results[1]

print(add_cols)
add_rows = pd.DataFrame({'column_name': add_cols,
'description': ["Medicare Average Age","Medicare % Female","Medicare Average HCC","Medicare Standardized Adjusted Cost per Person",
"% Medicare Beneficiaries"],
'demographic': [1, 1, 1, 0, 1],
'sdoh_raw': [0, 0, 0, 1, 0],
'outcome': [0]*5,
'sdoh_score': [0]*5,
'data_type': ['continuous','percentage','continuous','continuous','percentage'],
'used_sdoh_1': [0]*5,
'used_sdoh_2': [0]*5,
'used_sdoh_3': [0]*5,
'used_sdoh_4': [0]*5,
'used_sdoh_5': [0]*5,
'used_sdoh_6': [0, 0, 0, 1, 0],
'source': ['CMS']*5
})
data_dict = data_dict.append(add_rows)

data_dict.to_csv(os.path.join('data/data_dictionary.csv'), index = False)
data.to_csv(os.path.join(output, "geo_var_cleaned.csv"), index = False)