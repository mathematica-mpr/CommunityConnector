import os
try:
	os.chdir(os.path.join(os.getcwd(), '../../AHRQ/CommunityConnector/pipeline/codes/02_SDoH'))
	print(os.getcwd())
except:
	pass

from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.chrome.options import Options
options = Options()
# options.add_argument('--headless')
# options to try to improve performance
options.add_argument("--proxy-server='direct://'")
options.add_argument("--proxy-bypass-list=*")
options.add_argument('--blink-settings=imagesEnabled=false')

import time
from os import listdir
import regex as re
from stat import S_ISREG, ST_CTIME, ST_MODE, ST_MTIME
import shutil

dirpath = 'C:/Users/kskvoretz/Downloads/'
n_drive = 'N:/Transfer/KSkvoretz/AHRQ/data//02_SDoH/HCUP'
state = "Colorado"
website = "https://hcupnet.ahrq.gov"

# # Selections for pulling data from the portal
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

# test connection
def connect(website, timeout):
    # For this to work, need to move chromedriver.exe to python Scripts/ folder
    # i.e. C:\Users\kskvoretz\AppData\Local\Continuum\anaconda3\Scripts
    driver = webdriver.Chrome(chrome_options=options)
    driver.set_page_load_timeout(timeout)
    driver.delete_all_cookies()
    driver.get(website)
    driver.quit()

# Note: this is much faster at work than at home. May need to adjust the threshold at home or try on wired connection
print('connecting')
connect(website, 600)
print('done connecting')
#TODO: if selenium ends up working, use these sources to wait until page loads:
#https://stackoverflow.com/questions/17462884/is-selenium-slow-or-is-my-code-wrong

def hcup_pull(state, analysis_selection, classifier_selection, diagnosis_selection, num):

    """
    Navigates through hcup data portal and downloads data
    
    Args:
        state (string)
        analysis_selection (string)
        classifier_selection (string)
        diagnosis_selection (string)
        
    Returns:
        
    """
    
    driver = webdriver.Chrome(chrome_options=options)
    driver.delete_all_cookies()
    driver.get(website)
    time.sleep(4)
    
    create_analysis = driver.find_element_by_class_name("create-analysis")
    create_analysis.click()
    time.sleep(2)
    community = driver.find_element_by_id("DS_COMM")
    community.click()
    single_year = driver.find_element_by_id("YEAR_SINGLE")
    single_year.click()
    year = driver.find_element_by_class_name("dropdown-toggle")
    year.click()
    latest_year = driver.find_element_by_link_text("2016")
    latest_year.click()
    area = driver.find_element_by_class_name("bs-placeholder")
    area.click()

    # could do other states here if necessary
    state = driver.find_element_by_link_text(state)
    state.click()
    counties = driver.find_element_by_id("CL_COUNTY")
    counties.click()

    analysis = driver.find_element_by_id(analysis_selection)
    analysis.click()
    time.sleep(1)

    if analysis_selection == "DP":
        # I swear this first one used to work
        # classify = driver.find_element_by_class_name("bs-placeholder")
        # classify = driver.find_element_by_class_name("filter-option pull-left")
        # classify = driver.find_element_by_link_text("Choose a Classification")
        classify = driver.find_element_by_id("question-6")
        classify.click()
        classifier = driver.find_element_by_link_text(classifier_selection)
        classifier.click()
        time.sleep(2)
        diagnosis = driver.find_element_by_class_name("bs-placeholder")
        diagnosis.click()
        diagnosis_selection = driver.find_element_by_link_text(diagnosis_selection)
        diagnosis_selection.click()

    # If Quality Indicators --> Preventative/Pediatric --> Composite/Acute/Diabetes
    # If All Stays --> Create Analysis
    else:
        pass
    analysis = driver.find_element_by_class_name("btn-block")
    analysis.click()
    time.sleep(1)
    agree = driver.find_element_by_id("accept-dua")
    agree.click()
    time.sleep(2)
    # I swear this also used to work
    time.sleep(5)
    csv = driver.find_element_by_class_name("file-text-o")
    csv.click()
    time.sleep(1)
    
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

if __name__ == "__main__":
    num = 0
    for parameters in selections_list[:1]:
        print(parameters)   
        hcup_pull(state, parameters[0], parameters[1], parameters[2], num)
        num += 1