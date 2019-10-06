import pandas as pd
import numpy as np

datafile = "data/full_data.csv"

data = pd.read_csv(datafile)
print(data.shape)

# preliminary columns to keep
keep_phrases = ['FIPS','County','SEX_BY_AGE','RACE','MEDIAN_HOUSEHOLD_INCOME','PUBLIC_ASSISTANCE_INCOME_Estimate','GINI_INDEX',
'PER_CAPITA_INCOME','MEAN_USUAL_HOURS','HOUSING_UNITS','OCCUPANCY_STATUS','TENURE','BEDROOMS','MEDIAN_GROSS_RENT_AS_A_PERCENTAGE','MEDIAN_VALUE']
keep_vars = []
colnum = 0
fips_colnum = data.shape[1]
for col in data.columns.values:
    # keep everything after the FIPS column
    if col == "FIPS":
        fips_colnum = colnum
    # keep the columns that aren't by two variables to start
    if ("BY" not in col) or (col in ['SEX_BY_AGE_Estimate_Total','SEX_BY_AGE_Estimate_Total_Male','SEX_BY_AGE_Estimate_Total_Female']) or (colnum >= fips_colnum):
        if any(phrase in col for phrase in keep_phrases):
            keep_vars.append(col)
        elif colnum >= fips_colnum:
            keep_vars.append(col)
    colnum += 1

# are there any variables that don't have a ton of data? drop them if low coverage
nrows = data.shape[0]
for var in keep_vars:
    coverage = data[pd.notnull(data[var])].shape[0]/nrows
    # TODO: this is an arbitrary cutoff
    if coverage < 0.8:
        print(f"Dropping {var} due to low coverage ({round(coverage,2)*100}%)")
        keep_vars.remove(var)

# keep the sdoh_scores - these variables will be calculated in the next code
for i in range(1,7):
    keep_vars.append(f"sdoh_score{i}")
print(len(keep_vars))

# export all column names to a data dictionary with the following format:
# column_name|description|demographic|sdoh_raw|outcome|sdoh_score|data_type|used_sdoh_score_1|used_sdoh_score_2|used_sdoh_score_3|used_sdoh_score_4|used_sdoh_score_5|used_sdoh_score_6
pre_dict = pd.read_csv('data/columns.csv')
data_dictionary = pd.DataFrame({'column_name': keep_vars,
# will change the descriptions to something better later
'description': keep_vars})

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
for ix, row in data_dictionary.iterrows():
    if row['column_name'] == "FIPS":
        print(ix)
data_dictionary.drop(data_dictionary.index[[38,39]], inplace = True)

# change the description for the SDoH scores
data_dictionary.loc[data_dictionary['column_name'].str.contains('sdoh_score'),'description'] = ['Economic Stability','Neighborhood & Physical Environment',
'Education','Food','Community','Health Coverage']

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
def available_vars(data, data_dictionary, lkp_phrase, corr = False):
    print("")
    print(lkp_phrase)
    cols = data_dictionary.loc[data_dictionary['column_name'].str.lower().str.contains(lkp_phrase, regex = True), 'column_name']
    print(data[cols].describe())
    if corr:
        print("Correlations:")
        print(data[cols].corr())
    return cols

# view non-truncated dataframe
pd.set_option('display.max_colwidth', -1)
pd.set_option('display.max_columns', 30)

# For now, use the following variables:
# Demographics: Sex, race, median income, GINI, % rural, Life expectancy, infant mortality
sex_vars = available_vars(data, data_dictionary, 'sex|female')
dem_vars.append("% Female")

race_cols = available_vars(data, data_dictionary, 'race')
race_cols = np.setdiff1d(race_cols, ["RACE_Estimate_Total", "RACE_Estimate_Total_Two_or_more_races_Two_races_including_Some_other_race",
"RACE_Estimate_Total_Two_or_more_races_Two_races_excluding_Some_other_race_and_three_or_more_races"])
data[race_cols] = data[race_cols].apply(lambda x: x/data['RACE_Estimate_Total'])
data_dictionary['data_type'] = np.where(data_dictionary['column_name'].isin(race_cols), 'percentage', data_dictionary['data_type'])
dem_vars.extend(race_cols)

# median income, gini index
available_vars(data, data_dictionary,'income')
dem_vars.extend(['MEDIAN_HOUSEHOLD_INCOME_IN_THE_PAST_12_MONTHS_(IN_2017_INFLATION-ADJUSTED_DOLLARS)_Estimate_Median_household_income_in_the_past_12_months_(in_2017_inflation-adjusted_dollars)',
'GINI_INDEX_OF_INCOME_INEQUALITY_Estimate_Gini_Index'])

# % rural
available_vars(data, data_dictionary, 'rural')
dem_vars.append('% Rural')

# Life expectancy
available_vars(data, data_dictionary, "life")
dem_vars.append("Life Expectancy")

available_vars(data, data_dictionary, "population")
dem_vars.append("Population_x")

# How are all of the demographic variables correlated?
print(dem_vars)
print(data[dem_vars].corr())
# if no problematic correlations, flag all of these as demographic variables, and the rest as 0's
# rural and some races have higher correlations, as does mortality and income, but going to keep them all in for now
data_dictionary['demographic'] = np.where(data_dictionary['column_name'].isin(dem_vars), 1, 0)

# Economic Stability: % unemployed, % free or reduced lunch
available_vars(data, data_dictionary, "unemployed|free")
econ_vars.extend(['% Unemployed','% Free or Reduced Lunch'])
data_dictionary['used_sdoh_1'] = np.where(data_dictionary['column_name'].isin(econ_vars), 1, 0)

# Neighborhood: Housing units, Occupancy status, violent crime rate
available_vars(data, data_dictionary, "housing|occupancy|crime")
data['OCCUPANCY_STATUS_Estimate_Total_Vacant'] = data['OCCUPANCY_STATUS_Estimate_Total_Vacant']/data['OCCUPANCY_STATUS_Estimate_Total']
data_dictionary['data_type'] = np.where(data_dictionary['column_name'] == "OCCUPANCY_STATUS_Estimate_Total_Vacant", 'percentage', data_dictionary['data_type'])
neigh_vars.extend(['OCCUPANCY_STATUS_Estimate_Total_Vacant','Violent Crime Rate', '% Severe Housing Problems', '% Severe Housing Cost Burden'])
print(data[neigh_vars].corr())
data_dictionary['used_sdoh_2'] = np.where(data_dictionary['column_name'].isin(neigh_vars), 1, 0)

# Education: Graduation rate
available_vars(data, data_dictionary, "graduation|college")
edu_vars.extend(['Graduation Rate','% Some College'])
# data_dictionary['data_type'] = np.where(data_dictionary['column_name'] == "Graduation Rate", 'rate', data_dictionary['data_type'])
data_dictionary['used_sdoh_3'] = np.where(data_dictionary['column_name'].isin(edu_vars), 1, 0)

# Food: Food environment index, % food insecure
available_vars(data, data_dictionary, "food|insecure")
food_vars.extend(['Food Environment Index','% Food Insecure'])
data_dictionary['used_sdoh_4'] = np.where(data_dictionary['column_name'].isin(food_vars), 1, 0)

# Community: Hours Worked, mentall unhealthy
available_vars(data, data_dictionary, "hours|mental", True)
data['# Mental Health Providers'] = data['# Mental Health Providers']/data['Population_x']
data_dictionary['data_type'] = np.where(data_dictionary['column_name'] == "# Mental Health Providers", 'percentage', data_dictionary['data_type'])
comm_vars.extend(['MEAN_USUAL_HOURS_WORKED_IN_THE_PAST_12_MONTHS_FOR_WORKERS_16_TO_64_YEARS_Estimate_Total',
'Mentally Unhealthy Days','# Mental Health Providers'])
print(data[comm_vars].corr())
data_dictionary['used_sdoh_5'] = np.where(data_dictionary['column_name'].isin(comm_vars), 1, 0)

# Health Coverage: % Uninsured, PCP Rate, Dentist Rate, MH Rate
available_vars(data, data_dictionary, "uninsured|pcp|dentist|mh", True)
health_vars.extend(['% Uninsured_x', 'PCP Rate','Dentist Rate','MHP Rate'])
# data_dictionary['data_type'] = np.where(data_dictionary['column_name'].isin(['PCP Rate','Dentist Rate','MHP Rate']), 'rate', data_dictionary['data_type'])
data_dictionary['used_sdoh_6'] = np.where(data_dictionary['column_name'].isin(health_vars), 1, 0)

# flag all as sdoh_raw
sdoh_raw_vars = econ_vars
sdoh_raw_vars.extend(neigh_vars)
sdoh_raw_vars.extend(comm_vars)
sdoh_raw_vars.extend(food_vars)
sdoh_raw_vars.extend(health_vars)
sdoh_raw_vars.extend(edu_vars)
data_dictionary['sdoh_raw'] = np.where(data_dictionary['column_name'].isin(sdoh_raw_vars), 1, 0)

# # Outcome: % obese, % diabetic, kidney...
available_vars(data, data_dictionary, "obese|kidney|diabete")
# TODO: we need more kidney and diabetes data
outcome_vars.append('% Obese')
data_dictionary['outcome'] = np.where(data_dictionary['column_name'].isin(outcome_vars), 1, 0)

# TODO: more variables could be flagged as sdoh_raw than just the ones that go into the aggregate sdoh scores, if we want

data.to_csv('data/full_data_relative.csv')
data_dictionary.to_csv('data/data_dictionary.csv')