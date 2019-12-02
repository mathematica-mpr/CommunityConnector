from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.chrome.options import Options
options = Options()
# options.add_argument('--headless')
# options to try to improve performance
options.add_argument("--proxy-server='direct://'")
options.add_argument("--proxy-bypass-list=*")
options.add_argument('--blink-settings=imagesEnabled=false')

import time
import os
from os import listdir
import regex as re
from stat import S_ISREG, ST_CTIME, ST_MODE, ST_MTIME
import shutil
import pandas as pd

import sys
sys.path.insert(1, 'pipeline/scraping_codes')
from utilities import click_button, connect, move_from_downloads

dirpath = 'C:/Users/kskvoretz/Downloads/'
output = 'data/raw'
state = "Colorado"
website = "https://hcupnet.ahrq.gov"

# Selections for pulling data from the portal
selections_list = [['DP','Major Diagnostic Categories (MDC)', '11 Diseases & Disorders Of The Kidney & Urinary Tract']]
msdrg_selections = ['8 Simultaneous pancreas/kidney transplant',
                    '619 O.R. procedures for obesity w mcc',
                    '620 O.R. procedures for obesity w cc',
                    '652 Kidney transplant',
                    '656 Kidney & ureter procedures for neoplasm w mcc',
                    '657 Kidney & ureter procedures for neoplasm w cc',
                    '658 Kidney & ureter procedures for neoplasm w/o cc/mcc',
                    '659 Kidney & ureter procedures for non-neoplasm w mcc',
                    '660 Kidney & ureter procedures for non-neoplasm w cc',
                    '661 Kidney & ureter procedures for non-neoplasm w/o cc/mcc',
                    '686 Kidney & urinary tract neoplasms w mcc',
                    '687 Kidney & urinary tract neoplasms w cc',
                    '688 Kidney & urinary tract neoplasms w/o cc/mcc',
                    '689 Kidney & urinary tract infections w mcc',
                    '690 Kidney & urinary tract infections w/o mcc',
                    '695 Kidney & urinary tract signs & symptoms w mcc',
                    '696 Kidney & urinary tract signs & symptoms w/o mcc'
                   ]
drg = [["DP", "Medicare-Severity Diagnosis Related Groups (MS-DRG)", drg] for drg in msdrg_selections]
selections_list.extend(drg)
print(selections_list)

selections_df = pd.DataFrame(selections_list, columns = ["Analysis Selection", "Classification", "Diagnosis"])
selections_df.to_csv(os.path.join(output, "HCUP_selections.csv"))

def hcup_pull(state, analysis_selection, classifier_selection, diagnosis_selection, num):

    """
    Navigates through hcup data portal and downloads data
    
    Args:
        state (string)
        analysis_selection (string)
        classifier_selection (string)
        diagnosis_selection (string)
        num (integer): query number for exporting and saving data
        
    Returns:
        output csv moved into output folder
    """
    
    driver = webdriver.Chrome(options=options)
    driver.delete_all_cookies()
    driver.get(website)
    
    click_button(driver, "class", "create-analysis", 20)
    click_button(driver, "ID", "DS_COMM")
    click_button(driver, "ID", "YEAR_SINGLE")
    click_button(driver, "class", "dropdown-toggle")
    click_button(driver, "text", "2016")
    click_button(driver, "class", "bs-placeholder")
    click_button(driver, "text", state)
    click_button(driver, "ID", "CL_COUNTY")
    click_button(driver, "ID", analysis_selection)

    if analysis_selection == "DP":
        click_button(driver, "ID", "question-6")
        click_button(driver, "text", classifier_selection)
        click_button(driver, "class", "bs-placeholder", 15)

        if classifier_selection == "Medicare-Severity Diagnosis Related Groups (MS-DRG)":
            time.sleep(3)
            # search = driver.find_element_by_class_name("bs-searchbox")
            # search = driver.find_element_by_class_name("form-control")
            # search = driver.find_element_by_id("search-control") # element not interactible
            search = driver.find_element_by_class_name("task-modal")
            time.sleep(10)
            search = driver.find_element_by_xpath("/html/body/div[6]/div/div/input")
            search.send_keys(diagnosis_selection)
            search.send_keys(Keys.RETURN)
        else:
            click_button(driver, "text", diagnosis_selection)

    # If Quality Indicators --> Preventative/Pediatric --> Composite/Acute/Diabetes
    # If All Stays --> Create Analysis
    else:
        pass

    click_button(driver, "class", "btn-block")

    # Accepting & downloading CSV requires more than the standard click_button
    time.sleep(15)
    click_button(driver, "ID", "accept-dua")
    # sometimes file-text-o works...
    # csv_class = "file-text-o"
    csv_class = "export-csv"
    placeholder = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.CLASS_NAME, csv_class)))
    placeholder = WebDriverWait(driver, 20).until(EC.visibility_of_element_located((By.CLASS_NAME, csv_class)))
    time.sleep(30)
    csv = driver.find_element_by_class_name(csv_class)
    csv.click()
    
    # This file ends up in Downloads. Export the most recent Download to our data folder
    move_from_downloads(dirpath, "export", output, f"HCUP_{num}.csv")
    driver.quit()

if __name__ == "__main__":

    # Note: this is much faster at work than at home. At home, may need to adjust the threshold or try on wired connection
    # At work, 10 minute (600) threshold is more than enough
    # print('connecting')
    # connect(website, 600)
    # print('done connecting')

    start_num = 0
    num = start_num
    for parameters in selections_list[start_num:]:
        print(parameters)   
        hcup_pull(state, parameters[0], parameters[1], parameters[2], num)
        num += 1