from selenium import webdriver
from selenium.webdriver.chrome.options import Options
options = Options()
# options.add_argument('--headless')
# options to try to improve performance
options.add_argument("--proxy-server='direct://'")
options.add_argument("--proxy-bypass-list=*")
options.add_argument('--blink-settings=imagesEnabled=false')

import sys
sys.path.insert(1, 'pipeline/scraping_codes')
from utilities import click_button, move_from_downloads
import time
import pandas as pd
import os

downloads = 'C:/Users/kskvoretz/Downloads/'
output = 'data/raw/'
website = "https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Medicare-Geographic-Variation/GV_PUF.html"

driver = webdriver.Chrome(options=options)
driver.delete_all_cookies()
driver.get(website)

click_button(driver, "text","State/County Table - All Beneficiaries")
time.sleep(100)
move_from_downloads(downloads, "State-County-All-Table-2017",
output, "geo_var_data.zip")