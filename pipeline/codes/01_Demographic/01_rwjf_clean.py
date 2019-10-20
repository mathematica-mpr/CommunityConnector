import os
try:
	os.chdir(os.path.join(os.getcwd(), 'pipeline\codes\01_Demographic'))
	print(os.getcwd())
except:
	pass

import pandas as pd
import numpy as np
import os

read_state = 'Colorado'
n_drive = 'N:/Transfer/KSkvoretz/AHRQ/data/01_Demographic/RWJF'
output = 'data/cleaned/01_Demographic'

def replace_column_names(df):
    colnames = df.head(1).values
    colnames = list(colnames)[0]
    df.columns = colnames
    
    # drop the first row that contains the column names
    df.drop(df.index[0], inplace = True)
    return df

def concatenate_column_names(df):
    newcols = []
    for col in df.columns.values:
        if col in [0,'FIPS','State','County']:
            newcols.append(col)
        else:
            if col not in ['95% CI - Low','95% CI - High','Z-Score']:
                metric = col
                newcols.append(col)
            else:
                newcols.append(metric + "_" + col)
    return newcols

measure_data = pd.read_csv(os.path.join(n_drive, f'Measure_Data_{read_state}.csv'))
measure_data = replace_column_names(measure_data)
measure_data.columns = concatenate_column_names(measure_data)
measure_data.drop(['Population'], axis = 1, inplace = True)
print(measure_data.head())

addtl_data = pd.read_csv(os.path.join(n_drive, f'Additional_Data{read_state}.csv'))
addtl_data = replace_column_names(addtl_data)
addtl_data.columns = concatenate_column_names(addtl_data)
print(addtl_data.head())

# combine measure data with additional data
rwjf_data = pd.merge(measure_data, addtl_data, on = ['FIPS','State','County'])
assert((measure_data.shape[0]) == (addtl_data.shape[0]) == (rwjf_data.shape[0]))

rwjf_data.to_csv(os.path.join(output, 'RWJF_cleaned.csv'))


