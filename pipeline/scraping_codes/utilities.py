import censusdata
import math

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

def rwjf_replace_column_names(df):
    colnames = df.head(1).values
    colnames = list(colnames)[0]
    df.columns = colnames
    
    # drop the first row that contains the column names
    df.drop(df.index[0], inplace = True)
    return df

def rwjf_concatenate_column_names(df):
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

from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By

def click_button(driver, element_type, element_name, time_to_wait = 10):
    """
    Wait for element to be clickable, then find and click on the element

    Args: 
        driver
        element_type (string): class, ID, or text
        element_name (string)
        time_to_wait (integer)

    """

    if element_type == "class":
        placeholder = WebDriverWait(driver, time_to_wait).until(EC.element_to_be_clickable((By.CLASS_NAME, element_name)))
        button = driver.find_element_by_class_name(element_name)
    elif element_type == "ID":
        placeholder = WebDriverWait(driver, time_to_wait).until(EC.element_to_be_clickable((By.ID, element_name)))
        button = driver.find_element_by_id(element_name)
    elif element_type == "text":
        placeholder = WebDriverWait(driver, time_to_wait).until(EC.element_to_be_clickable((By.LINK_TEXT, element_name)))
        button = driver.find_element_by_link_text(element_name)
    button.click()

import regex as re
import os
from stat import S_ISREG, ST_CTIME, ST_MODE, ST_MTIME
import shutil

def move_from_downloads(orig_path, search_term, new_path, new_name):
    files = os.listdir(orig_path)
    files = [x for x in files if re.search(search_term, x)] 
    entries = (os.path.join(orig_path, fn) for fn in os.listdir(orig_path))
    entries = ((os.stat(path), path) for path in entries)
    # leave only regular files, insert creation date
    #NOTE: on Windows `ST_CTIME` is a creation date 
    #NOTE: use `ST_MTIME` to sort by a modification date
    entries = ((stat[ST_MTIME], path)
               for stat, path in entries if S_ISREG(stat[ST_MODE]))
    count = 0
    for cdate, path in sorted(entries, reverse=True):
        # keep only the first, most recent file name
        while count == 0:
            filename = os.path.basename(path)
            count += 1
            print(filename)

    shutil.move(orig_path + filename, os.path.join(new_path, new_name))

import pandas as pd

# this depends on RWJF data already being cleaned
def pull_population():
    pop = pd.read_csv('data/cleaned/01_Demographic/RWJF_cleaned.csv')
    pop = pop[['FIPS','Population']]
    pop.columns.values[1] = 'population'
    return pop
    
# Looking to choose variables that have good coverage and that aren't super correlated with one of the others
def available_vars(data, lkp_phrase, corr = False):
    print("")
    print(lkp_phrase)
    cols = data.filter(regex=lkp_phrase).columns.values
    print(data[cols].describe())
    if corr:
        print("Correlations:")
        print(data[cols].corr())
    return cols

import numpy as np

def remove_from_dict(data):
    data_dict = pd.read_csv('data/data_dictionary.csv')
    add_cols = data.columns.values
    add_cols = [c for c in add_cols if c != "FIPS"]
    # remove if they are already in it
    rest_cols = pd.DataFrame(np.setdiff1d(data_dict['column_name'], add_cols))
    rest_cols.columns = ["column_name"]
    pre_rows = data_dict.shape[0]
    data_dict = pd.merge(data_dict, rest_cols, on = "column_name")
    print(f"Dropped {pre_rows - data_dict.shape[0]} existing rows")
    return [data_dict, add_cols]

def custom_replace(col):
    return col.replace("% ","pct_").replace("< ","lt_").replace("/","_").replace("%","pct").replace(" ", "_").replace("(","").replace(")","").replace("-","").replace("__","_")

def fix_percentages(data_dictionary, data):

    pct_vars = data_dictionary[data_dictionary['data_type'] == 'percentage']['column_name']
    for col in pct_vars:
        if max(data[col]) < 1:
            print(col + " is being adjusted")
            print(max(data[col]))
            print(min(data[col]))
            data[col] = data[col] * 100
    return data

def check_negatives(data_dictionary, data):
    print("Any negative values in data?")
    for col in data_dictionary[data_dictionary['data_type'] != 'ID']['column_name']:
        if 'sdoh_score' not in col:
            if min(data[col]) < 0:
                print(col)