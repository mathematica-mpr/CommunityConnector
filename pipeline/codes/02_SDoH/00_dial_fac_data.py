import pandas as pd 
from sodapy import Socrata
from numpy import nanmean

client = Socrata("data.medicare.gov",
                 "WzCiIEqAIN57WaGqsXaeGyZmw")
results = client.get("23ew-n7w9", limit = 10000)

# Convert to pandas DataFrame
results_df = pd.DataFrame.from_records(results)

# TODO: potential other columns = offerings
results_df = results_df[['provider_number','five_star','state','county','_of_dialysis_stations','mortality_rate_facility','readmission_rate_facility']]

# use only colorado and prep county for merge to FIPS
results_df = results_df[results_df.state == "CO"]
results_df.drop(['state'], axis = 1,  inplace = True)
results_df['county'] = results_df['county'].str.lower()

# mean of all metrics
metric_cols = ['five_star','_of_dialysis_stations','mortality_rate_facility','readmission_rate_facility']
results_df[metric_cols] = results_df[metric_cols].apply(pd.to_numeric, errors = 'coerce')
grouped = results_df.groupby(['county']).agg({'five_star': nanmean,
'_of_dialysis_stations': nanmean,
'mortality_rate_facility': nanmean,
'readmission_rate_facility': nanmean}).reset_index()

# number of dialysis facilities
num_fac = results_df.groupby(['county']).count()['provider_number'].reset_index()

output = pd.merge(grouped, num_fac, on = 'county', how = 'outer')
output.columns = ['county','dial_fac_avg_rating','dial_fac_avg_stations','dial_fac_avg_mort','dial_fac_avg_readm','num_dial_facil']
print(output)

output.to_csv('data/raw/dial_fac_data.csv', index = False)