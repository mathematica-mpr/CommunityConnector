import pandas as pd
import numpy as np
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import remove_from_dict

data = pd.read_sas('data/raw/ahrf2019.sas7bdat')
# limit to Colorado only
data = data[data['f00008'] == b"Colorado"]
old_cols = data.columns.values

# columns to keep
# normalize by population
data['population'] = data['f1198418']
data['hosp_pp_rate'] = data['f0886817']
data['adm_pp_rate'] = data['f0890917']
data['kidn_hosp_pp_rate'] = data['f1407817']
# create a variable equal to Total MDs (F11072) + DOs (F14717) in general practice conducting patient care/ total population (F11984)
data['mds_dos_pp_rate'] = data['f1107217'] + data['f1471717']
# number of short term general hospitals (F08869) per total population
data['short_hosp_pp_rate'] = data['f0886917']
# number of community health centers (F15253) per total population
data['comm_hlth_cntrs_pp_rate'] = data['f1525319']
# % persons in deep poverty (F15419)
data['pct_deep_poverty'] = data['f1541913']
# % 18-64 without health insurance (F15498) - already have
# population density per square mile (F13876)
data['pop_dens'] = data['f1387610']
# housing unit density per square mile (F13877)
data['housing_dens'] = data['f1387710']
# % good air quality days (F15266)
data['pct_good_air'] = data['f1526618']
# elevation (F00811)
data['elevation'] = data['f0081176']

# will need to aggregate to this level, because it is actually
# at the census level
data['FIPS'] = data['f00012'].str.decode('utf-8')
new_cols = list(set(data.columns.values) - set(old_cols))
new_cols = [c for c in new_cols if c != "FIPS"]
print(new_cols)

sum_cols = [c for c in new_cols if 'pp_rate' in c]
sum_cols.append('population')
mean_cols = list(set(new_cols) - set(sum_cols))
print(sum_cols)
print(mean_cols)

grouped_sum = data.groupby(['FIPS']).sum()[sum_cols]
# others need to be averaged
# TODO: could be a weighted mean to be more accurate
grouped_mean = data.groupby(['FIPS']).sum()[mean_cols]
# then merge them back together

grouped = pd.merge(grouped_sum, grouped_mean, on = 'FIPS')

vars_used = [c for c in sum_cols if c != "population"]
grouped[vars_used] = grouped[vars_used].apply(lambda x: x/grouped['population'])

grouped.drop(['population'], axis = 1, inplace = True)
grouped.reset_index(inplace = True)

add_cols = grouped.columns.values
add_cols = [c for c in add_cols if c != "FIPS"]

data_dict = remove_from_dict(grouped)
data_dict['source'] = [""]*data_dict.shape[0]
print(data_dict)

add_rows = pd.DataFrame({'column_name': add_cols,
'description': ['Admissions per person','Kidney hospitals per person','Community Health Centers per person',
'MDs + DOs per person','Hospitals per person','Short-term hospitals per person','Population density',
'% Good air quality days','Housing density','% in deep poverty','Elevation'],
'demographic': [0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1],
'sdoh_raw': [0, 1, 1, 1, 1, 1, 0, 1, 1, 0, 0],
'outcome': [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
'sdoh_score': [0]*11,
'data_type': ['continuous','continuous','continuous','continuous','continuous','continuous','rate',
'percentage','rate','percentage','continuous'],
'used_sdoh_1': [0]*11,
'used_sdoh_2': [0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0],
'used_sdoh_3': [0]*11,
'used_sdoh_4': [0]*11,
'used_sdoh_5': [0]*11,
'used_sdoh_6': [0, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0],
'source': ['AHRF']*11
})
data_dict = data_dict.append(add_rows)
print(data_dict)
data_dict.to_csv('data/data_dictionary.csv', index = False)

grouped.to_csv('data/cleaned/02_SDoH/ahrf_cleaned.csv')