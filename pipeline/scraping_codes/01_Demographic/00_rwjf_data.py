from bs4 import BeautifulSoup, SoupStrainer
from urllib.request import urlopen, urlretrieve, quote
from urllib.parse import urljoin
import gzip
import pandas as pd
import numpy as np
import sys
sys.path.insert(1, 'pipeline/scraping_codes')
from utilities import rwjf_concatenate_column_names, rwjf_replace_column_names
import os

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

n_drive = 'N:/Transfer/KSkvoretz/AHRQ/data/01_Demographic/RWJF'
output = 'data/cleaned/01_Demographic'

measure_data = rwjf_replace_column_names(measure_data)
measure_data.columns = rwjf_concatenate_column_names(measure_data)
measure_data.rename(columns = {"% Uninsured": "pct_uninsured"}, inplace = True)

addtl_data = rwjf_replace_column_names(addtl_data)
addtl_data.columns = rwjf_concatenate_column_names(addtl_data)

# there are two columns called % uninsured
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
# Population_x is the population with some college education, Population_y is from Addtl data
rwjf_data['Population'] = rwjf_data['Population_y']
assert((measure_data.shape[0]) == (addtl_data.shape[0]) == (rwjf_data.shape[0]))

rwjf_data.to_csv(os.path.join(output, 'RWJF_cleaned.csv'))


