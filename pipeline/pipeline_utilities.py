from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.by import By

import regex as re
import os
from stat import S_ISREG, ST_CTIME, ST_MODE, ST_MTIME
import shutil
from selenium import webdriver
from selenium.webdriver.chrome.options import Options
options = Options()
options.add_argument('--headless')
# options to try to improve performance
options.add_argument("--proxy-server='direct://'")
options.add_argument("--proxy-bypass-list=*")
options.add_argument('--blink-settings=imagesEnabled=false')

import time
import pandas as pd
# view non-truncated dataframe
pd.set_option('display.max_colwidth', -1)
pd.set_option('display.max_columns', 30)

from io import StringIO
import requests

import numpy as np

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

# TODO: fix selenium.common.exceptions.SessionNotCreatedException: Message: session not created: This version
# of ChromeDriver only supports Chrome version 76
# https://stackoverflow.com/questions/57600228/sessionnotcreatedexception-message-session-not-created-this-version-of-chrome

def GeographicPUF(outdir, downloads = 'C:/Users/kskvoretz/Downloads/',
    website = "https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Geographic-Variation/GV_PUF.html"):

    driver = webdriver.Chrome(options=options)
    driver.delete_all_cookies()
    driver.get(website)

    click_button(driver, "text","State/County Table - All Beneficiaries [ZIP, 68MB]")
    time.sleep(300)
    move_from_downloads(downloads, "State-County-All-Table-2017",
    outdir, "geo_var_data.zip")

def OppAtlas(output, input = 'data/raw/opp_atlas_stay.csv',
    url="https://opportunityinsights.org/wp-content/uploads/2018/12/cty_covariates.csv"):
    
    s=requests.get(url).text

    data = pd.read_csv(StringIO(s), sep=",")
    data = data[['state','county','frac_coll_plus2010','rent_twobed2015','traveltime15_2010','ann_avg_job_growth_2004_2013']]
    data = data[data['state'] == 8]
    data.drop(['state'], axis = 1, inplace = True)
    data.columns.values[0] = "FIPS"

    # TODO: need to scrape this, but pulled data for now using the following selections:
    # Download the Data --> % Staying in Same Tract as Adults --> Counties
    # "All" in the subgroup selections below
    stay = pd.read_csv(input, encoding = "ISO-8859-1")
    stay['State'] = [re.split(', ',state,1)[1] for state in stay['Name']]
    stay['FIPS'] = [int(cty[-3:]) for cty in stay['cty']]
    stay = stay[stay['State'] == "CO"]
    stay = stay[['%_Staying_in_Same_Tract_as_Adults_rP_gP_pall','FIPS']]

    data = pd.merge(data, stay, on = "FIPS")

    print(output)
    data.to_csv(output, index = False)

def MergeCleaned(cleaned_drive, output, data_types = ['01_Demographic','02_SDoH','03_Outcome']):

    count = 0
    for t in data_types:
        cleaned_files = os.listdir(os.path.join(cleaned_drive, t))

        for file in cleaned_files:
            if "csv" in file:
                data = pd.read_csv(os.path.join(cleaned_drive, t, file))
                print(file)
                print(data.shape)

                data['FIPS'] = [int(str(fips)[-3:]) for fips in data['FIPS']]
                # make sure the file is unique by FIPS
                print(data['FIPS'].drop_duplicates().shape)
                data_columns = pd.DataFrame({'column_name': data.columns.values,
                    'type': t
                    })

                if count == 0:
                    full_data = data
                    columns = data_columns
                else:
                    full_data = pd.merge(full_data, data, on = 'FIPS', how = 'outer')
                    columns = columns.append(data_columns)
                print(f"Completed {file}")
                print(full_data.shape)
                print('')

                count += 1

    # now that we've merged on FIPS, add the 08 at the beginning for Colorado state code
    full_data['FIPS'] = ["08"+str(fips).zfill(3) for fips in full_data['FIPS']]
    # drop counties that have FIPS = 999 or 0
    full_data = full_data[full_data['FIPS'] != "08999"]
    full_data = full_data[full_data['FIPS'] != "08000"]
    full_data['County'] = [col + " County" for col in full_data['County']]

    print(full_data.shape)
    # Custom check for Colorado
    assert(full_data.shape[0] == 64)

    # add sdoh_score1-6 to columns
    sdoh_score_names = [f'sdoh_score{i}' for i in range(1,7)]
    columns = columns.append(pd.DataFrame({'column_name': sdoh_score_names,
    'type': ['aggregate']*6}))

    full_data.to_csv(output, index = False)

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

def check_low_coverage(data):
    nrows = data.shape[0]
    count = 0
    for var in data.columns.values:
        coverage = data[pd.notnull(data[var])].shape[0]/nrows
        # TODO: this is an arbitrary cutoff
        if coverage < 0.8:
            print(f"Dropping {var} due to low coverage ({round(coverage,2)*100}%)")
            data.drop([var], axis = 1, inplace = True)
            count += 1
    return data, count

def SelectVariables(input, output, data_dictionary = 'data/data_dictionary.csv'):

    data = pd.read_csv(input)
    print("Original shape of data: " + str(data.shape))
    data.columns = map(str.lower, data.columns)

    # keep variables from data dictionary - mostly created in cleaning codes
    data_dictionary = pd.read_csv(data_dictionary)
    data_dictionary.columns = map(str.lower, data_dictionary.columns)
    keep_cols = data_dictionary['column_name']
    keep_cols = [col for col in keep_cols if 'sdoh_score' not in col]
    # TODO: move to earlier cleaning
    data['med_2br_rent_per_med_inc'] = 100*data['rent_twobed2015']/(data['median_income']/12)
    data['state'] = "CO"

    print("Number of columns from data dictionary: " + str(len(keep_cols)))

    # doing this because I messed up the data dictionary - not sure if this is needed anymore
    data.columns = [custom_replace(col) for col in data.columns.values]

    data = data[keep_cols]
    assert(data.shape[1] == len(keep_cols))

    # are there any variables that don't have a ton of data? drop them if low coverage
    data, count = check_low_coverage(data)
    print("# columns dropped due to low coverage: " + str(count))

    # check to make sure all of the percentage variables are between 0-100, not 0-1
    print(data_dictionary.data_type.drop_duplicates())

    # checked rates too, none of those need adjusting as of now
    data = fix_percentages(data_dictionary, data)

    check_negatives(data_dictionary, data)
    data.loc[data['budget_health_info'] < 0,'budget_health_info'] = 0

    data.to_csv(output, index = False)

def ReduceDisplayVars(input, input_data_dictionary, output, output_data_dictionary):
    
    final_dict = pd.read_csv(input_data_dictionary)
    pd.set_option('max_rows', final_dict.shape[0])

    # flag variables to keep in the app
    # this could be adjusted based on the health outcomes in question
    final_dict['keep'] = 0
    final_dict.loc[pd.notnull(final_dict['sdoh_Category']), 'keep'] = 1
    final_dict.loc[final_dict['outcome'] == 1, 'keep'] = 1
    final_dict.loc[final_dict['sdoh_score'] == 1, 'keep'] = 1
    final_dict.loc[final_dict['data_type'] == 'ID', 'keep'] = 1

    # others to keep from Keri
    other_keep_vars = ['pct_lt_18','pct_65_and_over','pct_female','pct_hispanic',
    'pop_dens','pct_nonhispanic_white','pct_rural','life_expectancy',
    'race_estimate_total_black_or_african_american_alone','median_income',
    # KS adds:
    'pct_staying_in_same_tract_as_adults_rp_gp_pall', 'pct_only_english']
    race_vars = [col for col in final_dict.column_name if 'race_estimate' in col]
    other_keep_vars.extend(race_vars)
    other_keep_vars = list(set(other_keep_vars))

    final_dict.loc[final_dict.column_name.isin(other_keep_vars), 'keep'] = 1

    # check which ones we aren't keeping and add any additional
    print(final_dict[final_dict['keep'] == 0]['column_name'])

    # drop the RWJF outcomes in favor of the CDC outcomes
    final_dict.loc[(final_dict.outcome == 1) & (final_dict.source == "RWJF"), 'keep'] = 0

    # check the descriptions of the variables we are keeping
    final_dict = final_dict[final_dict.keep == 1]
    print(final_dict['description'])

    # keep only variables needed and output final data
    data = pd.read_csv(input)
    final_data = data[final_dict['column_name']]

    final_data.to_csv(output)
    final_dict.to_csv(output_data_dictionary, index = False)
