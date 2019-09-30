from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
from selenium.common.exceptions import TimeoutException
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

dirpath = 'C:/Users/kskvoretz/Downloads/'
n_drive = 'N:/Transfer/KSkvoretz/AHRQ/data//02_SDoH/HCUP'
state = "Colorado"
website = "https://hcupnet.ahrq.gov"

# Selections for pulling data from the portal
selections_list = [['DP','Major Diagnostic Categories (MDC)', '11 Diseases & Disorders Of The Kidney & Urinary Tract']]
msdrg_selections = ['8 Simultaneous pancreas/kidney trasnplant',
                    '619 O.R. procedures for obesity w mcc',
                    '620 O.R. procedures for obesity w cc',
                    '652 Kidney transplant',
                    '656 Kidney & ureter procedures for neoplasm w mcc',
                    '657 Kidney & ureter procedures for neoplasm w cc',
                    '658 Kideny & ureter procedures for neoplasm w/o cc/mcc',
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

def connect(website, timeout):
    """
    This function can be used to test the connection to the website. If it doesn't connect within timout seconds, the code will error out

    Args:   
        website (string): name of website to access
        timeout (integer): seconds to wait before page times out

    """
    # For this to work, need to move chromedriver.exe to python Scripts/ folder
    # i.e. C:\Users\kskvoretz\AppData\Local\Continuum\anaconda3\Scripts
    driver = webdriver.Chrome(chrome_options=options)
    driver.set_page_load_timeout(timeout)
    driver.delete_all_cookies()
    driver.get(website)
    driver.quit()

def click_button(element_type, element_name, time_to_wait):
    """
    Wait for element to be clickable, then find and click on the element

    Args: 
        element_type (string)
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
        
    """
    
    driver = webdriver.Chrome(chrome_options=options)
    driver.delete_all_cookies()
    driver.get(website)
    
    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.CLASS_NAME, "create-analysis")))
    create_analysis = driver.find_element_by_class_name("create-analysis")
    create_analysis.click()

    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.ID, "DS_COMM")))
    community = driver.find_element_by_id("DS_COMM")
    community.click()

    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.ID, "YEAR_SINGLE")))
    single_year = driver.find_element_by_id("YEAR_SINGLE")
    single_year.click()

    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.CLASS_NAME, "dropdown-toggle")))
    year = driver.find_element_by_class_name("dropdown-toggle")
    year.click()

    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.LINK_TEXT, "2016")))
    latest_year = driver.find_element_by_link_text("2016")
    latest_year.click()

    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.CLASS_NAME, "bs-placeholder")))
    area = driver.find_element_by_class_name("bs-placeholder")
    area.click()

    # could do other states here if necessary
    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.LINK_TEXT, state)))
    state = driver.find_element_by_link_text(state)
    state.click()

    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.ID, "CL_COUNTY")))
    counties = driver.find_element_by_id("CL_COUNTY")
    counties.click()

    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.ID, analysis_selection)))
    analysis = driver.find_element_by_id(analysis_selection)
    analysis.click()

    if analysis_selection == "DP":
        # I swear this first one used to work
        # classify = driver.find_element_by_class_name("bs-placeholder")
        # classify = driver.find_element_by_class_name("filter-option pull-left")
        # classify = driver.find_element_by_link_text("Choose a Classification")
        placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.ID, "question-6")))
        classify = driver.find_element_by_id("question-6")
        classify.click()

        placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.LINK_TEXT, classifier_selection)))
        classifier = driver.find_element_by_link_text(classifier_selection)
        classifier.click()

        placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.CLASS_NAME, "bs-placeholder")))
        diagnosis = driver.find_element_by_class_name("bs-placeholder")
        diagnosis.click()

        placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.LINK_TEXT, diagnosis_selection)))
        diagnosis_selection = driver.find_element_by_link_text(diagnosis_selection)
        diagnosis_selection.click()
    # If Quality Indicators --> Preventative/Pediatric --> Composite/Acute/Diabetes
    # If All Stays --> Create Analysis
    else:
        pass

    # placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.CLASS_NAME, "create-analysis")))
    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.CLASS_NAME, "btn-block")))
    analysis = driver.find_element_by_class_name("btn-block")
    analysis.click()

    # For some reason, accepting DUA and downloading CSV needs time.sleep
    # time.sleep(8)
    placeholder = WebDriverWait(driver, 10).until(EC.element_to_be_clickable((By.ID, "accept-dua")))
    agree = driver.find_element_by_id("accept-dua")
    agree.click()

    placeholder = WebDriverWait(driver, 20).until(EC.element_to_be_clickable((By.CLASS_NAME, "file-text-o")))
    placeholder = WebDriverWait(driver, 20).until(EC.visibility_of_element_located((By.CLASS_NAME, "file-text-o")))
    time.sleep(10)
    csv = driver.find_element_by_class_name("file-text-o")
    csv.click()
    
    # This file ends up in Downloads. Export the most recent Download to our data folder
    files = listdir(dirpath)
    files = [x for x in files if re.search("export", x)] 
    entries = (os.path.join(dirpath, fn) for fn in os.listdir(dirpath))
    entries = ((os.stat(path), path) for path in entries)
    # leave only regular files, insert creation date

    #NOTE: on Windows `ST_CTIME` is a creation date 
    #NOTE: use `ST_MTIME` to sort by a modification date
    entries = ((stat[ST_MTIME], path)
               for stat, path in entries if S_ISREG(stat[ST_MODE]))
    count = 0
    for cdate, path in sorted(entries, reverse=True):
        while count == 0:
            filename = os.path.basename(path)
            count += 1

    shutil.move(dirpath + filename, os.path.join(n_drive, f"HCUP_{num}.csv"))
    driver.quit()

if __name__ == "__main__":

    # Note: this is much faster at work than at home. At home, may need to adjust the threshold or try on wired connection
    print('connecting')
    connect(website, 600)
    print('done connecting')

    num = 0
    for parameters in selections_list:
        print(parameters)   
        hcup_pull(state, parameters[0], parameters[1], parameters[2], num)
        num += 1