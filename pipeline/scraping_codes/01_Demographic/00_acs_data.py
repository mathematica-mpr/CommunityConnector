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
from utilities import censusdata_pull
key = '***REMOVED***'
output = 'data/cleaned/01_Demographic'

# location of table shells where I"ve flagged which variables to use
n_drive = 'N:\Transfer\KSkvoretz\AHRQ\data\\01_Demographic\ACS'
table_shell = os.path.join(n_drive, 'ACS2017_Table_Shells.xlsx')
xl = pd.ExcelFile(table_shell)
table_shell_df = xl.parse(xl.sheet_names[0])
# variables I've flagged to use
use_vars = table_shell_df[table_shell_df.Use == 1]
print(use_vars[['TableID','Stub','Use']])
use_vars.to_csv(os.path.join(n_drive, 'ACS_variables.csv'))
variables = use_vars.TableID.tolist()

# # Method 2: Use the census data package
# Examples of functionality
censusdata.search('acs5', 2017, 'label','unemploy')
# censusdata.search('acs5', 2017, 'concept', 'education')
censusdata.printtable(censusdata.censustable('acs5',2017, 'B23025'))
censusdata.geographies(censusdata.censusgeo([('state','*')]), 'acs5', 2017)
censusdata.geographies(censusdata.censusgeo([('state', '08'), ('county', '*')]), 'acs5', 2017)

# doesn't seem like the C variables work, so remove them
variables = [var for var in variables if 'C' not in var]
variables = [var for var in variables if "B17002" not in var]

get_ipython().run_line_magic('time', '')
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

print(full_data.shape)
full_data.to_csv(os.path.join(output, 'ACS_cleaned.csv'))


