import pandas as pd
import os
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import available_vars, remove_from_dict
import numpy as np

data = pd.read_csv('data/raw/ACS_cleaned_pre.csv')
data.columns = map(str.lower, data.columns)
# columns to keep
print(data.columns.values)

race_cols = available_vars(data, 'race')
drop_race_cols = ["race_estimate_total", "race_estimate_total_two_or_more_races_two_races_including_some_other_race",
"race_estimate_total_two_or_more_races_two_races_excluding_some_other_race_and_three_or_more_races"]
race_cols = np.setdiff1d(race_cols, drop_race_cols)
data[race_cols] = data[race_cols].apply(lambda x: x/data['race_estimate_total'])

eng_vars = available_vars(data, 'english')
eng_vars = ['place_of_birth_by_language_spoken_at_home_and_ability_to_speak_english_in_the_united_states_estimate_total_speak_only_english',
 'place_of_birth_by_language_spoken_at_home_and_ability_to_speak_english_in_the_united_states_estimate_total_speak_other_languages',
 'place_of_birth_by_language_spoken_at_home_and_ability_to_speak_english_in_the_united_states_estimate_total_speak_other_languages_speak_english_very_well']
data[eng_vars] = data[eng_vars].apply(lambda x: x/data['place_of_birth_by_language_spoken_at_home_and_ability_to_speak_english_in_the_united_states_estimate_total'])

keep_vars = ['fips']
keep_vars.extend(race_cols)
keep_vars.extend(eng_vars)
keep_vars.extend(['median_household_income_in_the_past_12_months_(in_2017_inflation-adjusted_dollars)_estimate_median_household_income_in_the_past_12_months_(in_2017_inflation-adjusted_dollars)',
'gini_index_of_income_inequality_estimate_gini_index',
'mean_usual_hours_worked_in_the_past_12_months_for_workers_16_to_64_years_estimate_total'])

data = data[keep_vars]
data.columns.values[0] = 'FIPS'

results = remove_from_dict(data)
add_cols = results[1]
data_dict = results[0]
data_dict = data_dict[data_dict['source'] != "ACS"]

print(add_cols)

dem_vars = [1]*12
dem_vars.append(0)
sdoh_raw_vars = [0]*12
sdoh_raw_vars.append(1)
dtype_list = ['percentage']*10
dtype_list.extend(['continuous','continuous','continuous'])
sdoh_5_vars = [0]*12
sdoh_5_vars.append(1)

add_rows = pd.DataFrame({'column_name': add_cols,
'description': ['% American Indian or Alaskan Native','% Asian','% Black or African American','% Native Hawaiian or Pacific Islander',
'% Other race','% Two or More races','% White','% Speak Only English','% Speak Language other than English',
'% Speak Language other than English and Speak English well','Median Household Income','Gini Index of Income Inequality',
'Mean Hours Worked in past year'],
'demographic': dem_vars,
'sdoh_raw': sdoh_raw_vars,
'outcome': [0]*13,
'sdoh_score': [0]*13,
'data_type': dtype_list,
'used_sdoh_1': [0]*13,
'used_sdoh_2': [0]*13,
'used_sdoh_3': [0]*13,
'used_sdoh_4': [0]*13,
'used_sdoh_5': sdoh_5_vars,
'used_sdoh_6': [0]*13,
'source': ['ACS']*13
})
data_dict = data_dict.append(add_rows)
print(data_dict)
data_dict.to_csv('data/data_dictionary.csv', index = False)

# also output data
data.to_csv('data/cleaned/01_Demographic/ACS_cleaned.csv', index = False)