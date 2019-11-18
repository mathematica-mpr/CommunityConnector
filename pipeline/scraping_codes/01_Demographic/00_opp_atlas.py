from io import StringIO
import pandas as pd
import requests
import regex as re
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import remove_from_dict

url="https://opportunityinsights.org/wp-content/uploads/2018/12/cty_covariates.csv"
s=requests.get(url).text

data =pd.read_csv(StringIO(s), sep=",")
data = data[['state','county','frac_coll_plus2010','rent_twobed2015','traveltime15_2010','ann_avg_job_growth_2004_2013']]
data = data[data['state'] == 8]
data.drop(['state'], axis = 1, inplace = True)
data.columns.values[0] = "FIPS"

# need to scrape this, but pulled data for now using the following selections:
# Download the Data --> % Staying in Same Tract as Adults --> Counties
# "All" in the subgroup selections below
stay = pd.read_csv('data/raw/opp_atlas_stay.csv', encoding = "ISO-8859-1")
stay['State'] = [re.split(', ',state,1)[1] for state in stay['Name']]
stay['FIPS'] = [int(cty[-3:]) for cty in stay['cty']]
stay = stay[stay['State'] == "CO"]
stay = stay[['%_Staying_in_Same_Tract_as_Adults_rP_gP_pall','FIPS']]

data = pd.merge(data, stay, on = "FIPS")
data.to_csv('data/cleaned/01_Demographic/opp_atlas_cleaned.csv', index = False)

# add to data dictionary
results = remove_from_dict(data)
data_dict = results[0]
add_cols = results[1]
print(add_cols)

add_rows = pd.DataFrame({'column_name': add_cols,
'description': ["Percentage of people aged 25 or older who have at least a Bachelor's degree",
'Median gross rent for renter-occupied housing units with two bedrooms that pay cash rent',
'Share of workers 16 years and over who do not work at home whose commute is shorter than 15 minutes',
'Average annualized job growth rate over the time period 2004 to 2013',
'Fraction of children who grew up in this area who in 2015 still live in the same census tract'],
'demographic': [1]*5,
'sdoh_raw': [1]*5,
'outcome': [0]*5,
'sdoh_score': [0]*5,
'data_type': ['percentage','continuous','percentage','rate','percentage'],
'used_sdoh_1': [0, 0, 0, 1, 0],
'used_sdoh_2': [0, 0, 1, 0, 0],
'used_sdoh_3': [1, 0, 0, 0, 0],
'used_sdoh_4': [0]*5,
'used_sdoh_5': [0]*5,
'used_sdoh_6': [0]*5,
'source': ['Opportunity Atlas']*5
})
data_dict = data_dict.append(add_rows)
print(data_dict)
data_dict.to_csv('data/data_dictionary.csv', index = False)