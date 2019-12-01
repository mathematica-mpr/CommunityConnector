from bs4 import BeautifulSoup, SoupStrainer
from urllib.request import urlopen, urlretrieve, quote
from urllib.parse import urljoin
import gzip
import pandas as pd
import numpy as np

states = {"CO":"Colorado"}

def file_template(state, version):
    file = f'https://www.countyhealthrankings.org/sites/default/files/state/downloads/2019%20County%20Health%20Rankings%20{state}%20Data%20-%20v{version}.xls'
    return file

for lkp in states:
    read_state = states[lkp]
    state = read_state.replace(" ", "%20")
    
    # try to use the latest version first
    # otherwise, if there isn't a version 2, use version 1_0
    try:
        file = file_template(state, "2")
        measure_data = pd.read_excel(file, 'Ranked Measure Data')
    except:
        file = file_template(state, "1_0")
        measure_data = pd.read_excel(file, 'Ranked Measure Data')
        
    print(file)    
    addtl_data = pd.read_excel(file, 'Additional Measure Data')
    measure_data.to_csv(f'data/raw/Measure_Data_{read_state}.csv')
    addtl_data.to_csv(f'data/raw/Additional_Data{read_state}.csv')

print(read_state)

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
        if col in [0, 'FIPS','State','County']:
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
measure_data.drop(['Population',0], axis = 1, inplace = True)
measure_data.rename(columns = {"% Uninsured": "pct_uninsured"}, inplace = True)

addtl_data = pd.read_csv(os.path.join(n_drive, f'Additional_Data{read_state}.csv'))
addtl_data = replace_column_names(addtl_data)
addtl_data.columns = concatenate_column_names(addtl_data)
addtl_data.drop([0], axis = 1, inplace = True)
ind = 0
first = 0
for col in addtl_data.columns.values:
    if col == "% Uninsured":
        if first == 0:
            adult_uninsured_ind = ind
        else:
            child_uninsured_ind = ind
        first += 1
    ind += 1
addtl_data.columns.values[adult_uninsured_ind] = "pct_adult_uninsured"
addtl_data.columns.values[child_uninsured_ind] = "pct_child_uninsured"

# combine measure data with additional data
rwjf_data = pd.merge(measure_data, addtl_data, on = ['FIPS','State','County'])
assert((measure_data.shape[0]) == (addtl_data.shape[0]) == (rwjf_data.shape[0]))

rwjf_data.to_csv(os.path.join(output, 'RWJF_cleaned.csv'))


