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

from io import StringIO
import requests

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

def MergeCleaned(cleaned_drive, outdir, data_types = ['01_Demographic','02_SDoH','03_Outcome']):

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

    full_data.to_csv(os.path.join(outdir, 'full_data.csv'), index = False)
