from bs4 import BeautifulSoup, SoupStrainer
from urllib.request import urlopen, urlretrieve, quote
from urllib.parse import urljoin
import gzip
import pandas as pd

states = {"CO":"Colorado"}

def file_template(state, version):
    file = f'https://www.countyhealthrankings.org/sites/default/files/state/downloads/2019%20County%20Health%20Rankings%20{state}%20Data%20-%20v{version}.xls'
    return file

for lkp in states:
    read_state = states[lkp]
    state = read_state.replace(" ", "%20")
    
    # try to use the latest version first
    # otherwise, if there isn't a version 2, use version 1_0
    try:
        file = file_template(state, "2")
        measure_data = pd.read_excel(file, 'Ranked Measure Data')
    except:
        file = file_template(state, "1_0")
        measure_data = pd.read_excel(file, 'Ranked Measure Data')
        
    print(file)    
    addtl_data = pd.read_excel(file, 'Additional Measure Data')
    measure_data.to_csv(f'data/raw/Measure_Data_{read_state}.csv')
    addtl_data.to_csv(f'data/raw/Additional_Data{read_state}.csv')