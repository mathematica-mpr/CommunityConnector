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
key = 'b34f5dfe18f660a15a278a309760c38ef401b395'
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

def censusdata_pull(variable, acs_version = 'acs5', year = 2017, max_vars = 49.0):
    """
    Used to pull and merge data for multiple variables, since we are using much more than 50
    and 50 is the limit to pull from the API (According to the limit, it's 50, but it's actually 49)
    
    Args:
        variable (string): variable group name
        acs_version (string): 'acs5' for the 5-year survey
        year (int): 2017 is the most recent as of now
        max_vars (float): If API limit ever changes, we can adjust this default value from 49.0
        
    Returns:
        dataframe
    
    """
    
    # Create a list of all variable names found related to the input variable group
    census_dict = censusdata.censustable(acs_version,year,variable)
    unique_ids = list(census_dict.keys())
    
    # The API sets a limit of pulling 50 variables at a time
    num_vars = len(unique_ids)    
    # Number of loops we'll have to do to pull groups of 50 or less variables
    num_loops = math.ceil(num_vars/max_vars)
    
    # used to store the indices of the 50 variables to be pulled
    last = int(max_vars)
    first = 0
    for i in range(num_loops):
        
        print(len(unique_ids[first:last]))
        data = censusdata.download(acs_version, year,
        # pulling for Colorado by county
                              censusdata.censusgeo([('state', '08'), ('county', '*')]),
                              unique_ids[first:last])
        
        # rename columns from variable names
        new_colnames = {}
        for key, value in census_dict.items():
            new_name = value['concept'] + "_" + value['label']
            new_name = new_name.replace('!!', "_").replace(" ", "_")
            new_colnames[key] = new_name

        data.rename(columns = new_colnames, inplace = True)
        
        if i == 0:
            full_data = data
        else:
            # merge the data by county
            full_data = full_data.join(data, how = 'outer')
        
        # increment to 50 variables later
        last += int(max_vars)
        first = last-int(max_vars)
        if last > num_vars:
            last = num_vars
    
    return full_data

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


