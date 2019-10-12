import pandas as pd
# view non-truncated dataframe
pd.set_option('display.max_colwidth', -1)
pd.set_option('display.max_columns', 30)
import numpy as np

datafile = "data/full_data.csv"
data = pd.read_csv(datafile)
print(data.shape)
nrows = data.shape[0]

# are there any variables that don't have a ton of data? drop them if low coverage
count = 0
for var in data.columns.values:
    coverage = data[pd.notnull(data[var])].shape[0]/nrows
    # TODO: this is an arbitrary cutoff
    if coverage < 0.8:
        print(f"Dropping {var} due to low coverage ({round(coverage,2)*100}%)")
        data.drop([var], axis = 1, inplace = True)
        count += 1
print("# columns dropped due to low coverage: " + str(count))

# preliminary columns to keep
# TODO: others to consider: place_of_birth, geographical_mobility
keep_phrases = ['FIPS','County','SEX_BY_AGE','RACE','MEDIAN_HOUSEHOLD_INCOME','PUBLIC_ASSISTANCE_INCOME_Estimate','GINI_INDEX',
'PER_CAPITA_INCOME','MEAN_USUAL_HOURS','HOUSING_UNITS','OCCUPANCY_STATUS','TENURE','BEDROOMS',
'MEDIAN_GROSS_RENT_AS_A_PERCENTAGE','MEDIAN_VALUE']
keep_vars = []
colnum = 0
fips_colnum = data.shape[1]
for col in data.columns.values:
    # keep everything after the FIPS column
    if col == "FIPS":
        fips_colnum = colnum
    # keep the columns that aren't by two variables to start
    if (("BY" not in col) or (col in ['SEX_BY_AGE_Estimate_Total','SEX_BY_AGE_Estimate_Total_Male','SEX_BY_AGE_Estimate_Total_Female']) or
    (colnum >= fips_colnum) or
    ('RACE_Estimate_Total' in col) or ('MEANS_OF_TRANSPORTATION_TO_WORK_Estimate_Total' in col) or
    ('SEX_OF_WORKERS_BY_TRAVEL_TIME' in col and 'Male' not in col and 'Female' not in col) or
    ('MARITAL_STATUS_BY_SEX_BY_LABOR' in col and 'Male' not in col and 'Female' not in col)):
        if ('95%' not in col) and ('Z-Score' not in col) and ("#" not in col):
            if any(phrase in col for phrase in keep_phrases):
                keep_vars.append(col)
            elif colnum >= fips_colnum and col != "Unnamed: 0_y" and col != "0_x":
                keep_vars.append(col)
    colnum += 1

data = data[keep_vars]
print('Number of columns remaining: ' + str(len(data.columns)))

# Flag which variables to use in the scores
# Later, will look into variable reduction/impact on outcomes to determine the most important ones

dem_vars = []
econ_vars = []
neigh_vars = []
edu_vars = []
food_vars = []
comm_vars = []
health_vars = []
outcome_vars = []

# Looking to choose variables that have good coverage and that aren't super correlated with one of the others
def available_vars(data, lkp_phrase, corr = False):
    print("")
    print(lkp_phrase)
    cols = data.filter(regex=lkp_phrase).columns.values
    print(data[cols].describe())
    if corr:
        print("Correlations:")
        print(data[cols].corr())
    return cols

data.columns = map(str.lower, data.columns)
keep_vars = list(data.columns.values)
# keep the sdoh_scores - these variables will be calculated in the next code
for i in range(1,7):
    keep_vars.append(f"sdoh_score{i}")
print(len(keep_vars))

# For now, use the following variables:
# Demographics: Sex, race, median income, GINI, % rural, Life expectancy, infant mortality
sex_vars = available_vars(data, 'sex|female')
dem_vars.append("% female")
# drop the other variables
keep_vars = [c for c in keep_vars if ('sex_by_age' not in c)]
keep_vars = [c for c in keep_vars if ('mean_usual_hours_work' not in c or 'female' not in c)]

# TODO: could also use % race variables
race_cols = available_vars(data, 'race')
drop_race_cols = ["race_estimate_total", "race_estimate_total_two_or_more_races_two_races_including_some_other_race",
"race_estimate_total_two_or_more_races_two_races_excluding_some_other_race_and_three_or_more_races"]
race_cols = np.setdiff1d(race_cols, drop_race_cols)
data[race_cols] = data[race_cols].apply(lambda x: x/data['race_estimate_total'])
keep_vars = [c for c in keep_vars if c not in drop_race_cols]
dem_vars.extend(race_cols)

# median income, gini index
inc_cols = available_vars(data,'income')
print(inc_cols)
keep_inc_cols = ['median_household_income_in_the_past_12_months_(in_2017_inflation-adjusted_dollars)_estimate_median_household_income_in_the_past_12_months_(in_2017_inflation-adjusted_dollars)',
'gini_index_of_income_inequality_estimate_gini_index']
drop_inc_cols = np.setdiff1d(inc_cols, keep_inc_cols)
dem_vars.extend(keep_inc_cols)
keep_vars = [c for c in keep_vars if c not in drop_inc_cols]

# % rural
available_vars(data, 'rural')
dem_vars.append('% rural')

# Life expectancy
available_vars(data, "life")
dem_vars.append("life expectancy")
keep_vars = [c for c in keep_vars if c != "years of potential life lost rate"]

available_vars(data, "population")
dem_vars.append("population_x")
keep_vars = [c for c in keep_vars if c != "population_y"]

# How are all of the demographic variables correlated?
print(dem_vars)
print(data[dem_vars].corr())
# if no problematic correlations, flag all of these as demographic variables, and the rest as 0's
# rural and some races have higher correlations, as does mortality and income, but going to keep them all in for now

# Economic Stability: % unemployed, % free or reduced lunch
available_vars(data, "unemployed|free")
econ_vars.extend(['% unemployed','% free or reduced lunch'])

# Neighborhood: Housing units, Occupancy status, violent crime rate
available_vars(data, "housing|occupancy|crime")
data['occupancy_status_estimate_total_vacant'] = data['occupancy_status_estimate_total_vacant']/data['occupancy_status_estimate_total']
neigh_vars.extend(['occupancy_status_estimate_total_vacant','violent crime rate', '% severe housing problems',
'% severe housing cost burden', 'budget_environmental','budget_water'])
keep_vars = [c for c in keep_vars if c not in ["housing_units_estimate_total","occupancy_status_estimate_total",
"occupancy_status_estimate_total_occupied","annual average_violent crimes","severe housing cost burden"]]
print(data[neigh_vars].corr())

# Education: Graduation rate
available_vars(data, "graduation|college|school")
edu_vars.extend(['graduation rate','% some college'])

# Food: Food environment index, % food insecure
available_vars(data, "food|insecure")
food_vars.extend(['food environment index','% food insecure'])

# Community: Hours Worked, mentall unhealthy
available_vars(data, "hours|mental")
keep_vars = [c for c in keep_vars if (('mean_usual_hours' not in c) or ('male' not in c))]
keep_vars = [c for c in keep_vars if c not in ['# mental health providers','% frequent mental health distress']]
comm_vars.extend(['mean_usual_hours_worked_in_the_past_12_months_for_workers_16_to_64_years_estimate_total',
'mentally unhealthy days','budget_laboratory_svcs','budget_health_equity','budget_prevention','budget_planning'])
print(data[comm_vars].corr())

# Health Coverage: % Uninsured, PCP Rate, Dentist Rate, MH Rate
available_vars(data, "uninsured|pcp|dentist|mh")
keep_vars = [c for c in keep_vars if c not in ['% uninsured_y','% uninstured_y.1','other pcp rate']]
health_vars.extend(['% uninsured_x', 'pcp rate','dentist rate','mhp rate','budget_disease',
'budget_emergency','budget_health_svcs','budget_health_info','hosp_pp_rate','adm_pp_rate','kidn_hosp_pp_rate',
'dial_fac_avg_stations','dial_fac_avg_mort','dial_fac_avg_mort','dial_fac_avg_readm','dial_facil_pp_rate'])

# # Outcome: % obese, % diabetic, kidney...
available_vars(data, "obese|kidney|diabet|mortality")
# TODO: we need more kidney and diabetes data
outcome_vars.extend(['% obese','age-adjusted mortality','% diabetic'])

# # TODO: more variables could be flagged as sdoh_raw than just the ones that go into the aggregate sdoh scores, if we want

# export all column names to a data dictionary with the following format:
# column_name|description|demographic|sdoh_raw|outcome|sdoh_score|data_type|used_sdoh_score_1|used_sdoh_score_2|used_sdoh_score_3|used_sdoh_score_4|used_sdoh_score_5|used_sdoh_score_6
keep_vars = [c for c in keep_vars if 'tenure' not in c]
keep_vars = [c for c in keep_vars if 'bedrooms' not in c]
keep_vars = [c for c in keep_vars if c != "annual average violent crimes"]
keep_vars = [c for c in keep_vars if c != "0_y"]
pre_dict = pd.read_csv('data/columns.csv')
data_dictionary = pd.DataFrame({'column_name': keep_vars,
# will change the descriptions to something better later
'description': keep_vars})
# limit data to keep_vars
data = data[[c for c in keep_vars if 'sdoh_score' not in c]]

pre_dict['column_name'] = pre_dict.column_name.str.lower()
data_dictionary = pre_dict.merge(data_dictionary, on ='column_name', how = 'inner')
data_dictionary['demographic'] = [int(dtype == "01_Demographic") for dtype in data_dictionary['type']]
data_dictionary['sdoh_raw'] = [int(dtype == "02_SDoH") for dtype in data_dictionary['type']]
data_dictionary['outcome'] = [int(dtype == "03_Outcome") for dtype in data_dictionary['type']]
data_dictionary['sdoh_score'] = [int(dtype == "aggregate") for dtype in data_dictionary['type']]
data_dictionary.drop('type', axis = 1, inplace = True)

def data_type(c):
    if c in ['FIPS','County','State']:
        return 'ID'
    elif c in [f'sdoh_score{i}' for i in range(1,7)]:
        return 'aggregate'
    elif data[c].dtypes in ['float64','int64']:
        if all(data[c] <= 1) or (('%' in c and '95%' not in c) or (c.count("%") > 1)):
            return 'percentage'
        elif ('rate' in c.lower()) or ('ratio' in c.lower()):
            return 'rate'
        else:
            return 'continuous'
    else:
        return 'unknown'

data_dictionary['data_type'] = data_dictionary['column_name'].apply(data_type)
# remove rows with unknown
data_dictionary = data_dictionary[data_dictionary['data_type'] != "unknown"]

# columns for if the column was used in one of the sdoh scores
for i in range(6):
    data_dictionary[f'used_sdoh_{i+1}'] = 0

# remove additional FIPS columns
count = 0
fips_cols = []
for ix, row in data_dictionary.iterrows():
    if row['column_name'] == "fips":
        print(ix)
        if count > 0:
            fips_cols.append(ix)
        count += 1
data_dictionary.drop(data_dictionary.index[fips_cols], inplace = True)

data_dictionary['data_type'] = np.where(data_dictionary['column_name'].isin(race_cols), 'percentage', data_dictionary['data_type'])
data_dictionary['data_type'] = np.where(data_dictionary['column_name'] == "occupancy_status_estimate_total_vacant", 'percentage', data_dictionary['data_type'])
data_dictionary['data_type'] = np.where(data_dictionary['column_name'].isin(['pcp rate','dentist rate','mhp rate']), 'rate', data_dictionary['data_type'])

# change the description for the SDoH scores
data_dictionary.loc[data_dictionary['column_name'].str.contains('sdoh_score'),'description'] = ['Economic Stability','Neighborhood & Physical Environment','Education','Food','Community','Health Coverage']

data_dictionary['demographic'] = np.where(data_dictionary['column_name'].isin(dem_vars), 1, 0)
data_dictionary['used_sdoh_1'] = np.where(data_dictionary['column_name'].isin(econ_vars), 1, 0)
data_dictionary['used_sdoh_2'] = np.where(data_dictionary['column_name'].isin(neigh_vars), 1, 0)
data_dictionary['used_sdoh_3'] = np.where(data_dictionary['column_name'].isin(edu_vars), 1, 0)
data_dictionary['used_sdoh_4'] = np.where(data_dictionary['column_name'].isin(food_vars), 1, 0)
data_dictionary['used_sdoh_5'] = np.where(data_dictionary['column_name'].isin(comm_vars), 1, 0)
data_dictionary['used_sdoh_6'] = np.where(data_dictionary['column_name'].isin(health_vars), 1, 0)
data_dictionary['outcome'] = np.where(data_dictionary['column_name'].isin(outcome_vars), 1, 0)

# flag all as sdoh_raw
sdoh_raw_vars = econ_vars
sdoh_raw_vars.extend(neigh_vars)
sdoh_raw_vars.extend(comm_vars)
sdoh_raw_vars.extend(food_vars)
sdoh_raw_vars.extend(health_vars)
sdoh_raw_vars.extend(edu_vars)
data_dictionary['sdoh_raw'] = np.where(data_dictionary['column_name'].isin(sdoh_raw_vars), 1, 0)

print(data.shape)
print(data_dictionary.shape)
data.to_csv('data/full_data_relative.csv')
data_dictionary.to_csv('data/data_dictionary.csv')