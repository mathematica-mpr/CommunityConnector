from selenium import webdriver
from selenium.webdriver.chrome.options import Options
options = Options()
options.add_argument('--headless')
# options to try to improve performance
options.add_argument("--proxy-server='direct://'")
options.add_argument("--proxy-bypass-list=*")
options.add_argument('--blink-settings=imagesEnabled=false')

import sys
import time
import zipfile
import os
sys.path.insert(1, 'pipeline/codes')
from utilities import click_button, move_from_downloads

dirpath = 'C:/Users/kskvoretz/Downloads/'
raw_output = 'data/raw/'
website = "https://data.hrsa.gov/data/download"

driver = webdriver.Chrome(options=options)
driver.delete_all_cookies()
driver.get(website)

click_button(driver, "text", "Area Health Resources Files")
click_button(driver, "text", "SAS format")

time.sleep(10)
filename = "AHRF_2018_2019_SAS.zip"
move_from_downloads(dirpath, "AHRF_2018-2019", raw_output, filename)

# unzip file
with zipfile.ZipFile(os.path.join(raw_output, filename), 'r') as zip_ref:
    zip_ref.extractall(raw_output)