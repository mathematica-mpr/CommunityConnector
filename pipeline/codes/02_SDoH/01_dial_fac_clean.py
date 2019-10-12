import pandas as pd 

data = pd.read_csv('data/raw/dial_fac_data.csv')

mapping = pd.read_csv('data/raw/county_fips.csv')
mapping['county'] = mapping['CTYNAME'].str.replace(' County','').str.lower()
mapping = mapping[mapping.STNAME == "Colorado"]
mapping = mapping[['county','FIPS']].drop_duplicates()

print(data.shape)
data = pd.merge(data, mapping, on = 'county', how = 'left')
print(data.shape)
print(data)

data.to_csv('data/cleaned/dial_fac_cleaned.csv', index = False)