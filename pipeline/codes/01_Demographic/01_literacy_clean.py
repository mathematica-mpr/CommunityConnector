import pandas as pd 
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import remove_from_dict

data = pd.read_csv('data/raw/literacy_raw.csv')
data = data[data['FIPS'] != 8000]
data = data[['FIPS','% Lacking basic literacy']]
data['pct_lack_basic_literacy'] = data['% Lacking basic literacy']/100
data.drop(['% Lacking basic literacy'], axis = 1, inplace = True)
print(data.head())
print(data.shape)

results = remove_from_dict(data)
data_dict = results[0]
add_cols = results[1]
print(add_cols)

add_rows = pd.DataFrame({'column_name': add_cols,
'description': ['Those lacking Basic prose literacy skills include those who scored Below Basic in prose and those who could not be tested due to language barriers.'],
'demographic': [1],
'sdoh_raw': [0],
'outcome': [0],
'sdoh_score': [0],
'data_type': ['percentage'],
'used_sdoh_1': [0],
'used_sdoh_2': [0],
'used_sdoh_3': [0],
'used_sdoh_4': [0],
'used_sdoh_5': [0],
'used_sdoh_6': [0],
'source': ['NCES']
})
data_dict = data_dict.append(add_rows)
print(data_dict)
data_dict.to_csv('data/data_dictionary.csv', index = False)

data.to_csv('data/cleaned/01_Demographic/literacy_cleaned.csv', index = False)