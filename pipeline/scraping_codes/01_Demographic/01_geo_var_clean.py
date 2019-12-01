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
data.columns = map(str.lower, data.columns)
data.columns.values[0] = 'FIPS'
print(data.head())

data.to_csv(os.path.join(output, "geo_var_cleaned.csv"), index = False)