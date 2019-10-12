import pandas as pd

data = pd.read_sas('data/raw/ahrf2019.sas7bdat')
# limit to Colorado only
data = data[data['f00008'] == b"Colorado"]

# columns we want:

# MD's by specialty? - couldn't actually find nephrologists (kidney)
# not sure what else would be useful - maybe Public Health?
# TODO: ask Keri

# normalize by population
data['population'] = data['f1198418']

# health facilities total
data['hosp_pp_rate'] = data['f0886817']
data['adm_pp_rate'] = data['f0890917']
data['kidn_hosp_pp_rate'] = data['f1407817']

# all utilization - inpatient days?
# all expenditures - medicare costs?

# will need to aggregate to this level, because it is actually
# at the census level
data['FIPS'] = data['f00012'].str.decode('utf-8')
grouped = data.groupby(['FIPS']).sum()[['hosp_pp_rate','adm_pp_rate','kidn_hosp_pp_rate','population']]

vars_used = ['hosp_pp_rate','adm_pp_rate','kidn_hosp_pp_rate']
grouped[vars_used] = grouped[vars_used].apply(lambda x: x/grouped['population'])

grouped.drop(['population'], axis = 1, inplace = True)
grouped.reset_index(inplace = True)

print(grouped.head())

grouped.to_csv('data/cleaned/02_SDoH/ahrf_cleaned.csv')