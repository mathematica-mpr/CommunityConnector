import pandas as pd 
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import remove_from_dict

data = pd.read_csv('data/raw/cdphe.csv')
data['overobese_pct'] = data['overobese_pct']/100
print(data.head())
print(data.shape)

results = remove_from_dict(data)
data_dict = results[0]
add_cols = results[1]
print(add_cols)

add_rows = pd.DataFrame({'column_name': add_cols,
'description': ['Age-Adjusted Diabetes Hospitalization Rate Per 100,000 Persons',
'Percent of Adults who are Overweight'],
'demographic': [0]*2,
'sdoh_raw': [0]*2,
'outcome': [1]*2,
'sdoh_score': [0]*2,
'data_type': ['rate','percentage'],
'used_sdoh_1': [0]*2,
'used_sdoh_2': [0]*2,
'used_sdoh_3': [0]*2,
'used_sdoh_4': [0]*2,
'used_sdoh_5': [0]*2,
'used_sdoh_6': [0]*2,
'source': ['CDPHE']*2
})
data_dict = data_dict.append(add_rows)
print(data_dict)
data_dict.to_csv('data/data_dictionary.csv', index = False)

data.to_csv('data/cleaned/03_Outcome/cdphe_cleaned.csv', index = False)