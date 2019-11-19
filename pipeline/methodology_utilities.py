import pandas as pd
import numpy as np
import copy

def use_sdoh_normalize(data_dictionary, data, sdoh_score_num):
        # https://medium.com/@rrfd/standardize-or-normalize-examples-in-python-e3f174b65dfc
        # normalize variables to get all values between 0 and 1
        # TODO: consider standardizing instead, to get values centered around 0?
        # the outliers still remain visible
        # then take weighted average using pct variance explained from PCA
        
        spca_dict_data = data_dictionary[data_dictionary['sdoh_Category'] == sdoh_score_num]

        cols = list(spca_dict_data['Variable_Name'])
        print(cols)
        weights = list(spca_dict_data['weight'])

        # make sure all columns are found in dictionary and data
        assert(len(cols) == len(list(set(data_dictionary['column_name']).intersection(cols))) == len(list(set(data.columns.values).intersection(cols))))

        # standardize/normalize the variables
        # x = (data[cols] - data[cols].mean())/data[cols].std()
        x = (data[cols] - data[cols].min())/(data[cols].max() - data[cols].min())
        
        # because higher is better, need to make sure that larger numbers mean the county is stronger in that area
        flip_cols = list(data_dictionary[data_dictionary['higher_better'] == 0]['column_name'])
        flip_cols = list(set(flip_cols).intersection(cols))
        print("Lower is stronger cols:")
        print(flip_cols)
        
        if len(flip_cols) > 0:
            x[flip_cols] = 1-x[flip_cols]
        
        # weight them
        avgs = np.dot(x, weights)

        return avgs

def econ_adjust(econ_cor, data, i):
    correlation = econ_cor[i-1]
    new_score = (correlation * data[f'sdoh_score_{i}']/data['sdoh_score_1']) + (1 - correlation) * data[f'sdoh_score_{i}']
    # TODO: function this
    new_score = (new_score - new_score.min())/(new_score.max() - new_score.min())
    return new_score

# TODO: same as in pipeline_utilities
def custom_replace(col):
    return col.replace("% ","pct_").replace("< ","lt_").replace("/","_").replace("%","pct").replace(" ", "_").replace("(","").replace(")","").replace("-","").replace("__","_")

def FinalDictionary(spca_dictionary, output_data_dictionary, input_data_dictionary = 'data/data_dictionary.csv'):
    
    data_dictionary = pd.read_csv(input_data_dictionary)
    spca_dict = pd.read_csv(spca_dictionary)
    spca_dict = spca_dict[pd.notnull(spca_dict.sdoh_Category)]
    spca_dict['Loading_abs'] = spca_dict['Loading'].abs()

    # Create the weightings
    # find percentage of total loading in each sdoh/PC grouping
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

    data_dictionary.column_name = [custom_replace(col) for col in data_dictionary.column_name]

    # merge and output data dictionary
    inter_dict = pd.merge(data_dictionary, spca_dict, left_on = 'column_name', right_on = 'Variable_Name', how = 'left')
    inter_dict.drop(['Variable_Name','total_loading','pct_loading','total_variance_explained','pct_var','Loading_abs'], axis = 1, inplace = True)
    print(inter_dict.columns.values)
    
    inter_dict.to_csv(output_data_dictionary, index = False)

def SdohScores(input, input_data_dictionary, output):

    # read in data and final dictionary
    data = pd.read_csv(input)
    data_dictionary = pd.read_csv(input_data_dictionary)

    # number of columns in data should equal to number of variables in data dictionary
    assert(data.shape[1] == data_dictionary[~data_dictionary['column_name'].str.contains('sdoh_score')].shape[0])

    # replace missing values with the mean in order for the dot product to work below
    # TODO: consider another way of doing this
    data = data.fillna(data.mean())

    for i in range(1,7):
        print(i)
        data[f'sdoh_score_{i}'] = use_sdoh_normalize(data_dictionary, data, i)

    # are any SDoH scores too correlated
    cor = data[[f'sdoh_score_{i}' for i in range(1,7)]].corr()
    print(cor)
    econ_cor = cor['sdoh_score_1']
    print(econ_cor)

    # adjust for economic score
    for i in range(2,7):
        data[f'sdoh_score_{i}'] = econ_adjust(econ_cor, data, i)
    # check new correlations
    cor = data[[f'sdoh_score_{i}' for i in range(1,7)]].corr()
    print(cor)

    data.columns = [custom_replace(col) for col in data.columns.values]
    print(data.columns.values[:5])
    print(data_dictionary.column_name[:5])

    data.to_csv(output, index = False)