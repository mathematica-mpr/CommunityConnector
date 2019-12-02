# Resources: 
# Code lists, definitions, accuracy: https://www.census.gov/programs-surveys/acs/technical-documentation/code-lists.html
# Gitter help: https://gitter.im/uscensusbureau/general
# Variables: https://api.census.gov/data/2017/acs/acs5/variables.html
# available datasets: https://api.census.gov/data.html
# available geographies: https://api.census.gov/data/2009/acs5/geography.html
# instructional PPT: https://www.census.gov/content/dam/Census/programs-surveys/acs/guidance/training-presentations/06212017_ACS_Census_API.pdf

import requests
import urllib.request
import time
import pandas as pd
import json
import censusdata
import math
import os
from pathlib import Path
import numpy as np
import sys
sys.path.insert(1, 'pipeline/scraping_codes')
from utilities import censusdata_pull, available_vars
key = 'b34f5dfe18f660a15a278a309760c38ef401b395'

output = 'data/cleaned/01_Demographic'
# location of table shells where I"ve flagged which variables to use
input_drive = 'data/raw'

table_shell = os.path.join(input_drive, 'ACS2017_Table_Shells.xlsx')
xl = pd.ExcelFile(table_shell)
table_shell_df = xl.parse(xl.sheet_names[0])
# variables I've flagged to use
use_vars = table_shell_df[table_shell_df.Use == 1]
print(use_vars[['TableID','Stub','Use']])
use_vars.to_csv(os.path.join(input_drive, 'ACS_variables.csv'))
variables = use_vars.TableID.tolist()

# Use the census data package
# Examples of functionality
censusdata.search('acs5', 2017, 'label','unemploy')
# censusdata.search('acs5', 2017, 'concept', 'education')
censusdata.printtable(censusdata.censustable('acs5',2017, 'B23025'))
censusdata.geographies(censusdata.censusgeo([('state','*')]), 'acs5', 2017)
censusdata.geographies(censusdata.censusgeo([('state', '08'), ('county', '*')]), 'acs5', 2017)

# doesn't seem like the C variables work, so remove them
variables = [var for var in variables if 'C' not in var]
variables = [var for var in variables if "B17002" not in var]

# loop through all variables and merge data together
count = 0
for variable in variables:
    print(variable)
    data = censusdata_pull(variable)
    
    if count == 0:
        full_data = data
    else:
        full_data = full_data.join(data, how = 'outer')
    count += 1

full_data.columns.values[0] = "location"
full_data['FIPS'] = [location[-3:] for location in full_data['location']]
print(full_data['FIPS'])
full_data.drop(columns = "location", inplace = True)

full_data.columns = map(str.lower, full_data.columns)

# determine which columns to keep
print(full_data.columns.values)
keep_vars = ['fips']

race_cols = available_vars(full_data, 'race')
drop_race_cols = ["race_estimate_total", "race_estimate_total_two_or_more_races_two_races_including_some_other_race",
"race_estimate_total_two_or_more_races_two_races_excluding_some_other_race_and_three_or_more_races"]
race_cols = np.setdiff1d(race_cols, drop_race_cols)
full_data[race_cols] = full_data[race_cols].apply(lambda x: x/full_data['race_estimate_total'])
keep_vars.extend(race_cols)

eng_vars = available_vars(full_data, 'english')
eng_vars = ['place_of_birth_by_language_spoken_at_home_and_ability_to_speak_english_in_the_united_states_estimate_total_speak_only_english',
 'place_of_birth_by_language_spoken_at_home_and_ability_to_speak_english_in_the_united_states_estimate_total_speak_other_languages',
 'place_of_birth_by_language_spoken_at_home_and_ability_to_speak_english_in_the_united_states_estimate_total_speak_other_languages_speak_english_very_well']
full_data[eng_vars] = full_data[eng_vars].apply(lambda x: x/full_data['place_of_birth_by_language_spoken_at_home_and_ability_to_speak_english_in_the_united_states_estimate_total'])
new_eng_vars = ['pct_only_english','pct_other_languages','pct_other_languages_eng_vwell']
full_data[new_eng_vars] = full_data[eng_vars]
keep_vars.extend(new_eng_vars)

available_vars(full_data, 'occupancy')
full_data['pct_vacant'] = full_data['occupancy_status_estimate_total_vacant']/full_data['occupancy_status_estimate_total']
keep_vars.append('pct_vacant')

available_vars(full_data, 'median_value')
full_data['median_value'] = full_data['median_value_(dollars)_estimate_median_value_(dollars)']
full_data['median_income'] = full_data['median_household_income_in_the_past_12_months_(in_2017_inflation-adjusted_dollars)_estimate_median_household_income_in_the_past_12_months_(in_2017_inflation-adjusted_dollars)']
full_data['mean_hours'] = full_data['mean_usual_hours_worked_in_the_past_12_months_for_workers_16_to_64_years_estimate_total']
keep_vars.extend(['median_income',
'gini_index_of_income_inequality_estimate_gini_index',
'mean_hours','median_value'])

full_data = full_data[keep_vars]
full_data.columns.values[0] = 'FIPS'

full_data.to_csv(os.path.join(output, 'ACS_cleaned.csv'), index = False)


