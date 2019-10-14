import pandas as pd
import numpy as np

data = pd.read_csv('data/full_data_relative.csv')
data_dictionary = pd.read_csv('data/data_dictionary.csv')

# will cluster on these columns in the Shiny app
# Euclidean distance or some sort of multi-variate distance used in clustering
# TODO: this methodology in the next code
print(data_dictionary[data_dictionary['demographic'] == 1])

def use_sdoh_normalize(sdoh_score_num):
    # https://medium.com/@rrfd/standardize-or-normalize-examples-in-python-e3f174b65dfc
    # normalize variables to get all values between - and 1
    # TODO: consider standardizing instead, to get values centered around 0?
    # the outliers still remain visible
    # then take the average
    
    cols = list(data_dictionary[data_dictionary[f'used_sdoh_{sdoh_score_num}'] == 1]['column_name'])

    # x = (data[cols] - data[cols].mean())/data[cols].std()
    x = (data[cols] - data[cols].min())/(data[cols].max() - data[cols].min())
    
    # TODO: update this
    # because higher is better, need to make sure that larger numbers mean the county is stronger in that area
    no_flip_cols = ['graduation rate','% some college','food environment index','pcp rate','dentist rate','mhp rate',
    'budget_environmental','budget_water','budget_health_equity','budget_laboratory_svcs','budget_planning','budget_prevention',
    'mentally unhealthy days', 'budget_disease','budget_emergency','budget_health_info','budget_health_svcs',
    'hosp_pp_rate','kidn_hosp_pp_rate','dial_fac_avg_rating','dial_fac_avg_stations','dial_facil_pp_rate']
    flip_cols = np.setdiff1d(x.columns.values, no_flip_cols)
    print("Lower is stronger cols:")
    print(flip_cols)
    
    if len(flip_cols) > 0:
        x[flip_cols] = 1-x[flip_cols]

    avgs = x.mean(axis = 1)
    return avgs

for i in range(1,7):
    print(i)
    data[f'sdoh_score_{i}'] = use_sdoh_normalize(i)

# are any SDoH scores too correlated
print(data[[f'sdoh_score_{i}' for i in range(1,7)]].corr())
# not really! woohoo!
# the most correlated ones are 5/6, 1/3, and 1/5
# which are: community + health, economic + education, and economic + community. makes sense!

data.to_csv('data/data_sdoh_scores.csv', index = False)
