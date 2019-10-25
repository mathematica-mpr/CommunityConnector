# Coverage is way too low, so can't use this

import pandas as pd 
pd.set_option('display.max_colwidth', -1)
pd.set_option('display.max_rows', -1)
pd.set_option('display.max_columns', 30)
from sodapy import Socrata
import censusgeocode as cg

client = Socrata("data.cms.gov",
                 "WzCiIEqAIN57WaGqsXaeGyZmw")
results = client.get("97k6-zzx3", limit = 300000)
data = pd.DataFrame.from_records(results)

# limit to colorado
data = data[data['provider_state'] == "CO"]

# limit to the drgs we need
drgs = data["drg_definition"].drop_duplicates()
# keep_drgs = [drg for drg in drgs if 'DIABETES' in drg] # too low of coverage
keep_drgs.extend([drg for drg in drgs if 'RENAL' in drg])
keep_drgs.extend([drg for drg in drgs if 'KIDNEY' in drg])
keep_drgs = pd.DataFrame({'drg_definition': keep_drgs})
data = pd.merge(data, keep_drgs, on = 'drg_definition')
print(data.shape)

def get_fips(row):
    try:
        result = cg.address(row['provider_street_address'], city=row['provider_city'], state=row['provider_state'], zipcode=row['provider_zip_code'])
        fips = result[0]['geographies']['2010 Census Blocks'][0]['COUNTY']
    except:
        print(result)
        fips = ""
    return fips

data['FIPS'] = data.apply(get_fips, axis = 1)
# 23 don't have a FIPS

data.to_csv('data/raw/ipp_data.csv')
