from selenium import webdriver
from selenium.webdriver.chrome.options import Options
options = Options()
# options.add_argument('--headless')
# options to try to improve performance
options.add_argument("--proxy-server='direct://'")
options.add_argument("--proxy-bypass-list=*")
options.add_argument('--blink-settings=imagesEnabled=false')

import sys
import time
import zipfile
import os
sys.path.insert(1, 'pipeline/scraping_codes')
from utilities import click_button, move_from_downloads
import pandas as pd
import numpy as np

dirpath = 'C:/Users/kskvoretz/Downloads/'
raw_output = 'data/raw/'
website = "https://data.hrsa.gov/data/download"

driver = webdriver.Chrome(options=options)
driver.delete_all_cookies()
driver.get(website)

click_button(driver, "text", "Area Health Resources Files")
click_button(driver, "text", "SAS format")

time.sleep(100)
filename = "AHRF_2018_2019_SAS.zip"
move_from_downloads(dirpath, "AHRF_2018-2019", raw_output, filename)

# unzip file
with zipfile.ZipFile(os.path.join(raw_output, filename), 'r') as zip_ref:
    zip_ref.extractall(raw_output)
time.sleep(10)

data = pd.read_sas('data/raw/ahrf2019.sas7bdat')
# limit to Colorado only
data = data[data['f00008'] == b"Colorado"]
old_cols = data.columns.values

# columns to keep
data['population'] = data['f1198418']

# naming these pp_rate because we will divide by total population later
data['hosp_pp_rate'] = data['f0886817']
data['adm_pp_rate'] = data['f0890917']
data['kidn_hosp_pp_rate'] = data['f1407817']
# create a variable equal to Total MDs (F11072) + DOs (F14717) in general practice conducting patient care/ total population (F11984)

# MDS:  F12129-17 
# DOs:  F13882-17 
# compare with PCPs = primary care provider/physician
# 

# general practice
data['mds_dos_pp_rate'] = (data['f1107217'] + data['f1471717'])
# number of short term general hospitals (F08869) per total population
data['short_hosp_pp_rate'] = data['f0886917']
# number of community health centers (F15253) per total population
data['comm_hlth_cntrs_pp_rate'] = data['f1525319']

# % persons in deep poverty (F15419)
data['pct_deep_poverty'] = data['f1541913']
# % 18-64 without health insurance (F15498) - already have
# population density per square mile (F13876)
data['pop_dens'] = data['f1387610']
# housing unit density per square mile (F13877)
data['housing_dens'] = data['f1387710']
# % good air quality days (F15266)
data['pct_good_air'] = data['f1526618']
# elevation (F00811)
data['elevation'] = data['f0081176']

# will need to aggregate to this level, because it is actually
# at the census level
data['FIPS'] = data['f00012'].str.decode('utf-8')
new_cols = list(set(data.columns.values) - set(old_cols))
new_cols = [c for c in new_cols if c != "FIPS"]
print(new_cols)

# some columns need to be summed, some columns need to be averaged
sum_cols = [c for c in new_cols if 'pp_rate' in c]
sum_cols.append('population')
mean_cols = list(set(new_cols) - set(sum_cols))
print("Columns to be summed:")
print(sum_cols)
print("Columns to be averaged:")
print(mean_cols)

grouped_sum = data.groupby(['FIPS']).sum()[sum_cols]
# TODO: could be a weighted mean to be more accurate
grouped_mean = data.groupby(['FIPS']).mean()[mean_cols]

# then merge them back together
grouped = pd.merge(grouped_sum, grouped_mean, on = 'FIPS')

# divide sum columns by population to make the 'pp_rate'
vars_used = [c for c in sum_cols if c != "population"]
grouped[vars_used] = grouped[vars_used].apply(lambda x: x/grouped['population'])

grouped.drop(['population'], axis = 1, inplace = True)
grouped.reset_index(inplace = True)

# change to per 100,000 people for certain variables used
grouped['hosp_pp_rate'] = grouped['hosp_pp_rate'] * 100000
grouped['mds_dos_pp_rate'] = grouped['mds_dos_pp_rate'] * 100000

add_cols = grouped.columns.values
add_cols = [c for c in add_cols if c != "FIPS"]

grouped.to_csv('data/cleaned/02_SDoH/ahrf_cleaned.csv')