import pandas as pd 
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import remove_from_dict

data = pd.read_csv('data/raw/cdphe.csv')
data['overobese_pct'] = data['overobese_pct']/100
print(data.head())
print(data.shape)

data.to_csv('data/cleaned/03_Outcome/cdphe_cleaned.csv', index = False)