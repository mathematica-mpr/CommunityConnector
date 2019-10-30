import pandas as pd
import numpy as np

data = pd.read_csv('data/full_data_relative.csv')
data_dictionary = pd.read_csv('data/data_dictionary.csv')

# number of columns in data should equal number of variables in data dictionary
assert(data.shape[1] == data_dictionary[~data_dictionary['column_name'].str.contains('sdoh_score')].shape[0])

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
    no_flip_cols = list(data_dictionary[(data_dictionary[f'used_sdoh_{sdoh_score_num}'] == 1) &
    (data_dictionary['higher_better'] == 1)]['column_name'])
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
# especially make sure economics doesn't drive the others - somehow control for economic score??

# TODO: also check variation in outcome by score

def custom_replace(col):
    return col.replace("% ","pct_").replace("< ","lt_").replace("/","_").replace("%","pct").replace(" ", "_").replace("(","").replace(")","").replace("-","").replace("__","_")

data.columns = [custom_replace(col) for col in data.columns.values]
data_dictionary.column_name = [custom_replace(col) for col in data_dictionary.column_name]
print(data.columns.values[:5])
print(data_dictionary.column_name[:5])

data.to_csv('data/final_data.csv', index = False)
data_dictionary.to_csv('data/intermediate_data_dictionary.csv', index = False)
