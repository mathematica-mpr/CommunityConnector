import pandas as pd
pd.set_option('display.max_colwidth', -1)

final_dict = pd.read_csv('data/inter_data_dictionary.csv')
pd.set_option('max_rows', final_dict.shape[0])

# flag variables to keep in the app
# this could be adjusted based on the health outcomes in question
final_dict['keep'] = 0
final_dict.loc[pd.notnull(final_dict['sdoh_Category']), 'keep'] = 1
final_dict.loc[final_dict['outcome'] == 1, 'keep'] = 1
final_dict.loc[final_dict['sdoh_score'] == 1, 'keep'] = 1
final_dict.loc[final_dict['data_type'] == 'ID', 'keep'] = 1

# others to keep from Keri
other_keep_vars = ['pct_lt_18','pct_65_and_over','pct_female','pct_hispanic',
'pop_dens','pct_nonhispanic_white','pct_rural','life_expectancy',
'race_estimate_total_black_or_african_american_alone','median_income',
# KS adds:
'pct_staying_in_same_tract_as_adults_rp_gp_pall', 'pct_only_english']
race_vars = [col for col in final_dict.column_name if 'race_estimate' in col]
other_keep_vars.extend(race_vars)
other_keep_vars = list(set(other_keep_vars))

final_dict.loc[final_dict.column_name.isin(other_keep_vars), 'keep'] = 1

# check which ones we aren't keeping and add any additional
print(final_dict[final_dict['keep'] == 0]['column_name'])

# drop the RWJF outcomes in favor of the CDC outcomes
final_dict.loc[(final_dict.outcome == 1) & (final_dict.source == "RWJF"), 'keep'] = 0

# check the descriptions of the variables we are keeping
print(final_dict[final_dict.keep == 1]['description'])

final_dict.to_csv('data/final_data_dictionary.csv', index = False)