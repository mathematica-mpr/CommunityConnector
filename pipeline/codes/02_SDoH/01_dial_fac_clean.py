import pandas as pd 
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import pull_population

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

print(data.shape)
print(data)

data.to_csv('data/cleaned/dial_fac_cleaned.csv', index = False)