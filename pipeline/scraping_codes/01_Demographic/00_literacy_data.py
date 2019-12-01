# will need to scrape from here eventually: https://nces.ed.gov/naal/estimates/StateEstimates.aspx
# for now, manually downloaded excel sheet for Colorado - All Counties - 2003

import pandas as pd 
import sys
sys.path.insert(1, 'pipeline/scraping_codes')
from utilities import remove_from_dict

data = pd.read_csv('data/raw/literacy_raw.csv')
data = data[data['FIPS'] != 8000]
data = data[['FIPS','% Lacking basic literacy']]
data['pct_lack_basic_literacy'] = data['% Lacking basic literacy']
data.drop(['% Lacking basic literacy'], axis = 1, inplace = True)
print(data.head())
print(data.shape)

data.to_csv('data/cleaned/01_Demographic/literacy_cleaned.csv', index = False)