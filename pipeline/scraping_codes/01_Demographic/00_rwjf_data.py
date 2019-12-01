from bs4 import BeautifulSoup, SoupStrainer
from urllib.request import urlopen, urlretrieve, quote
from urllib.parse import urljoin
import gzip
import pandas as pd

# ## Loop through states and download data from available sheets:
# #### 1. Outcomes & Factor Rankings - no
# #### 2. Outcomes & Factor SubRankings - no
# #### 3. Ranked Measure Data - yes, raw data
# #### 4. Additional Measure Data - yes, raw data
# #### 5./6. tabs on sources for reference: Ranked Measure Sources & Years and Addtl Measure Sources & Years

states = {"AL":"Alabama","AK":"Alaska","AZ":"Arizona","AR":"Arkansas","CA":"California","CO":"Colorado","CT":"Connecticut","DE":"Delaware","FL":"Florida","GA":"Georgia","HI":"Hawaii","ID":"Idaho","IL":"Illinois","IN":"Indiana","IA":"Iowa","KS":"Kansas","KY":"Kentucky","LA":"Louisiana","ME":"Maine","MD":"Maryland","MA":"Massachusetts","MI":"Michigan","MN":"Minnesota","MS":"Mississippi","MO":"Missouri","MT":"Montana","NE":"Nebraska","NV":"Nevada","NH":"New Hampshire","NJ":"New Jersey","NM":"New Mexico","NY":"New York","NC":"North Carolina","ND":"North Dakota","OH":"Ohio","OK":"Oklahoma","OR":"Oregon","PA":"Pennsylvania","RI":"Rhode Island","SC":"South Carolina","SD":"South Dakota","TN":"Tennessee","TX":"Texas","UT":"Utah","VT":"Vermont","VA":"Virginia","WA":"Washington","WV":"West Virginia","WI":"Wisconsin","WY":"Wyoming"}

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
    measure_data.to_csv(f'N:/Transfer/KSkvoretz/AHRQ/data/01_Demographic/RWJF/Measure_Data_{read_state}.csv')
    addtl_data.to_csv(f'N:/Transfer/KSkvoretz/AHRQ/data/01_Demographic/RWJF/Additional_Data{read_state}.csv')