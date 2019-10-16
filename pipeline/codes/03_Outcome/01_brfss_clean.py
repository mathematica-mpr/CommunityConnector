# Coverage is really low, can't use

import pandas as pd 

data = pd.read_feather('data/raw/BRFSS.feather')
data = data[data['Locationabbr'] == "CO"]
data = data[pd.notnull(data['Data_value'])]
# will need to dedup by year
# for now, just limit to 2012
print(data.shape)

data = data[data['Year'] == 2011]
print(data.shape)
data.drop(['Class','Topic','State','Locationabbr','Locationdesc','Question','Sample_Size','Year'], axis = 1, inplace = True)

print(data.pivot(index = 'fips_full', columns = 'Response', values = 'Data_value'))