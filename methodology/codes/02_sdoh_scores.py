import pandas as pd
import numpy as np
import copy

data = pd.read_csv('data/full_data_relative.csv')
data_dictionary = pd.read_csv('data/data_dictionary.csv')
spca_dict = pd.read_csv('data/DictionaryPostSPCA.csv')
spca_dict = spca_dict[pd.notnull(spca_dict.sdoh_Category)]

# number of columns in data should equal number of variables in data dictionary
assert(data.shape[1] == data_dictionary[~data_dictionary['column_name'].str.contains('sdoh_score')].shape[0])

# Create the weightings
# find percentage of total loading in each sdoh/PC grouping
spca_dict['Loading_abs'] = spca_dict['Loading'].abs()
tot_loadings = spca_dict.groupby(['sdoh_Category','PC_Number']).sum().reset_index()[['sdoh_Category','PC_Number','Loading_abs']]
tot_loadings.rename(columns = {"Loading_abs": "total_loading"}, inplace = True)
spca_dict = pd.merge(spca_dict, tot_loadings, on = ['sdoh_Category',"PC_Number"])
spca_dict['pct_loading'] = spca_dict['Loading_abs']/spca_dict['total_loading']

# find the percentage of total variance explained in each sdoh/PC grouping
tot_var = spca_dict[['sdoh_Category','PC_Number','Variance_Explained']].drop_duplicates().groupby(['sdoh_Category']).sum().reset_index()[['sdoh_Category','Variance_Explained']]
tot_var.rename(columns = {"Variance_Explained": "total_variance_explained"}, inplace = True)
spca_dict = pd.merge(spca_dict, tot_var, on = ['sdoh_Category'])
spca_dict['pct_var'] = spca_dict['Variance_Explained']/spca_dict['total_variance_explained']
spca_dict['weight'] = spca_dict['pct_var']*spca_dict['pct_loading']
# all should be one!
print(spca_dict.groupby(['sdoh_Category']).sum().reset_index()['weight'])

# replace missing values with the mean in order for the dot product to work below
# TODO: consider another way of doing this
data = data.fillna(data.mean())

def use_sdoh_normalize(sdoh_score_num):
    # https://medium.com/@rrfd/standardize-or-normalize-examples-in-python-e3f174b65dfc
    # normalize variables to get all values between - and 1
    # TODO: consider standardizing instead, to get values centered around 0?
    # the outliers still remain visible
    # then take weighted average using pct variance explained from PCA
    
    spca_dict_data = spca_dict[spca_dict['sdoh_Category'] == sdoh_score_num]

    cols = list(spca_dict_data['Variable_Name'])
    print(cols)
    weights = list(spca_dict_data['weight'])

    # make sure all columns are found in dictionary and data
    assert(len(cols) == len(list(set(data_dictionary['column_name']).intersection(cols))) == len(list(set(data.columns.values).intersection(cols))))

    # standardize the variables
    # x = (data[cols] - data[cols].mean())/data[cols].std()
    x = (data[cols] - data[cols].min())/(data[cols].max() - data[cols].min())
    
    # TODO: update this
    # because higher is better, need to make sure that larger numbers mean the county is stronger in that area
    flip_cols = list(data_dictionary[data_dictionary['higher_better'] == 0]['column_name'])
    flip_cols = list(set(flip_cols).intersection(cols))
    print("Lower is stronger cols:")
    print(flip_cols)
    
    if len(flip_cols) > 0:
        x[flip_cols] = 1-x[flip_cols]
    
    # weight them
    avgs = np.dot(x, weights)
    # avgs = x.mean(axis = 1)

    return avgs

for i in range(1,7):
    print(i)
    data[f'sdoh_score_{i}'] = use_sdoh_normalize(i)

# are any SDoH scores too correlated
print(data[[f'sdoh_score_{i}' for i in range(1,7)]].corr())
# especially check for economic score (1)
# highly correlated (>.4): 1/3, 1/5, 1/6 = econ & education, econ & community, econ & health care system
# 2/3: neighborhood & education
# 3/4: education & food
# 4/5: food & community

# with just an average, the most correlated ones are 5/6, 1/3, and 1/5
# which are: community + health, economic + education, and economic + community. makes sense!

# TODO: also check variation in outcome by score
outcome_cols = list(data_dictionary[data_dictionary['outcome'] == 1]['column_name'])
print(outcome_cols)
for i in range(1,7):
    cols = copy.deepcopy(outcome_cols)
    cols.append(f"sdoh_score_{i}")
    print("")
    print(f"sdoh_score_{i}")
    print(data[cols].corr()[f"sdoh_score_{i}"])
    data['quartile'] = np.ceil(data[f"sdoh_score_{i}"]/0.25)
    cols.append('quartile')
    print(data[cols].groupby(['quartile']).mean())

# highly correlated (>0.4): 1 & pct diabetic, diab hosp rate adj, overobese, diabetes pct
# 2 & pct diabetic, chronic kidney, pct obese,  diab hosp rate, overobese
# 3 & pct diabetic, pct obese, diabetes pct, diab hosp rate adj, overobese
# 4 & none
# 5 & diabetes pct, diab hosp rate
# 6 & none

# TODO: flag final variables used in final data dictionary

# # def custom_replace(col):
# #     return col.replace("% ","pct_").replace("< ","lt_").replace("/","_").replace("%","pct").replace(" ", "_").replace("(","").replace(")","").replace("-","").replace("__","_")

# # data.columns = [custom_replace(col) for col in data.columns.values]
# # data_dictionary.column_name = [custom_replace(col) for col in data_dictionary.column_name]
# # print(data.columns.values[:5])
# # print(data_dictionary.column_name[:5])

# # data.to_csv('data/final_data.csv', index = False)
# # data_dictionary.to_csv('data/final_data_dictionary.csv', index = False)
