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

def pull_population():
    pop = pd.read_csv('data/cleaned/01_Demographic/RWJF_cleaned.csv')
    pop = pop[['FIPS','Population_x']]
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