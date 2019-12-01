import requests
import json
import pandas as pd 
import sys
sys.path.insert(1, 'pipeline/codes')

response = requests.get("https://opendata.arcgis.com/datasets/a27b6ee55fe3406fa5f2798ff87c0183_2.geojson")
json = response.json()

def get_keys(dl, keys_list):
    if isinstance(dl, dict):
        keys_list += dl.keys()
        map(lambda x: get_keys(x, keys_list), dl.values())
    elif isinstance(dl, list):
        map(lambda x: get_keys(x, keys_list), dl)

keys = []
get_keys(json, keys)
print(keys)

fips = []
rates = []
for i in range(0, len(json['features'])):
    county_data = json['features'][i]['properties']
    fips.append(county_data['COUNTY_FIPS'])
    rates.append(county_data['DIABETES_ADJRATE'])

data = pd.DataFrame({'FIPS': fips, 'diab_hosp_rate_adj': rates})
print(data.head())

# also include overweight data
response = requests.get("https://opendata.arcgis.com/datasets/3702d9b48efb49a8870bf0e375ea3817_9.geojson")
json = response.json()

fips = []
rates = []
for i in range(0, len(json['features'])):
    county_data = json['features'][i]['properties']
    fips.append(county_data['County_Name'])
    rates.append(county_data['OverweightObese_County_Regional_Estimate'])
over_data = pd.DataFrame({'County': fips, 'overobese_pct': rates})
over_data = over_data.drop_duplicates()
over_data['overobese_pct'] = [pct.split('Estimate', 1)[-1].split('%', 1)[0] for pct in over_data['overobese_pct']]
print(over_data.head())
print(over_data.shape)

# merge FIPS to over_data
pop = pd.read_csv('data/cleaned/01_Demographic/RWJF_cleaned.csv')
pop = pop[pd.notnull(pop['County'])]
pop = pop[['FIPS','County']]
over_data = pd.merge(over_data, pop, on = "County")
over_data['FIPS'] = [str(fips)[-3:] for fips in over_data['FIPS']]
print(over_data.shape)
print(over_data.head())

# merge data to over_data
data = pd.merge(data, over_data, on = "FIPS")
data.drop(['County'], axis = 1, inplace = True)
print(data.head())
print(data.shape)

data.to_csv('data/cleaned/03_Outcome/cdphe_cleaned.csv', index = False)