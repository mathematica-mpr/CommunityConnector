import pandas as pd
pd.set_option('display.max_colwidth', -1)
import os
import numpy as np

input_drive = 'data/raw/'
output = 'data/cleaned/02_SDoH/'

# join raw data with fips code
raw_data = pd.read_csv(os.path.join(input_drive, 'raw_budget.csv'))
raw_data.drop(['Vendor Name'], axis = 1, inplace = True)

# county-fips mapping from: https://raw.githubusercontent.com/plotly/datasets/master/minoritymajority.csv
mapping = pd.read_csv(os.path.join(input_drive, 'county_fips.csv'))
mapping['County'] = mapping['CTYNAME'].str.replace(' County','')
mapping = mapping[mapping.STNAME == "Colorado"]
mapping = mapping[['FIPS','County']]

data = pd.merge(raw_data, mapping, on = 'County', how = 'inner')
data.drop(['County'], axis = 1, inplace = True)
rows = data.shape[0]
assert(raw_data.shape[0] == rows)

# make all budget amounts percentage
totals = data.groupby(['FIPS']).sum().reset_index()
totals.columns.values[1] = 'Total Budget'

data = pd.merge(data, totals, on = 'FIPS')
data['Sum of Amount'] = data['Sum of Amount']/data['Total Budget']
data.drop(['Total Budget'], axis = 1, inplace = True)

# pivot data from long to wide
print(data['Division Name'].drop_duplicates())
# shorten division first
abbrevs = ['administrative','air','disease','emergency','environmental','waste',
'health_svcs','laboratory_svcs','health_equity','prevention','planning','water','health_info']
# add budget prefix to all new column names
abbrevs = ['budget_' + a for a in abbrevs]

data = pd.pivot_table(data, index = 'FIPS', columns = 'Division Name', values = 'Sum of Amount',
 aggfunc = np.sum)
data.fillna(0, inplace = True)
data.columns = abbrevs
data.reset_index(inplace = True)
print(data.head())

data.to_csv(os.path.join(output, 'budget_cleaned.csv'))