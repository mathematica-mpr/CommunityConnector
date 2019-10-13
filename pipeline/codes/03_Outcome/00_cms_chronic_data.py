from selenium import webdriver
from selenium.webdriver.chrome.options import Options
options = Options()
# options.add_argument('--headless')
# options to try to improve performance
options.add_argument("--proxy-server='direct://'")
options.add_argument("--proxy-bypass-list=*")
options.add_argument('--blink-settings=imagesEnabled=false')

import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import click_button, move_from_downloads
import time
import pandas as pd
import os

downloads = 'C:/Users/kskvoretz/Downloads/'
output = 'data/raw/'
website = "https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Chronic-Conditions/CC_Main.html"

driver = webdriver.Chrome(options=options)
driver.delete_all_cookies()
driver.get(website)

click_button(driver, "text","Prevalence State/County Level: All Beneficiaries by Age, 2007-2017 [ZIP, 20MB]")
time.sleep(40)
move_from_downloads(downloads, "CC_Prev_State_County",
output, "cms_prev_data.csv")

click_button(driver, "text","Spending County Level: All Beneficiaries, 2007-2017 [ZIP, 18MB]")
time.sleep(60)
move_from_downloads(downloads, "CC_Spend_County",
output, "cms_spend_data.csv")