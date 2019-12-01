from io import StringIO
import pandas as pd
import requests
import regex as re
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import remove_from_dict

url="https://opportunityinsights.org/wp-content/uploads/2018/12/cty_covariates.csv"
s=requests.get(url).text

data =pd.read_csv(StringIO(s), sep=",")
data = data[['state','county','frac_coll_plus2010','rent_twobed2015','traveltime15_2010','ann_avg_job_growth_2004_2013']]
data = data[data['state'] == 8]
data.drop(['state'], axis = 1, inplace = True)
data.columns.values[0] = "FIPS"

# need to scrape this, but pulled data for now using the following selections:
# Download the Data --> % Staying in Same Tract as Adults --> Counties
# "All" in the subgroup selections below
stay = pd.read_csv('data/raw/opp_atlas_stay.csv', encoding = "ISO-8859-1")
stay['State'] = [re.split(', ',state,1)[1] for state in stay['Name']]
stay['FIPS'] = [int(cty[-3:]) for cty in stay['cty']]
stay = stay[stay['State'] == "CO"]
stay = stay[['%_Staying_in_Same_Tract_as_Adults_rP_gP_pall','FIPS']]

data = pd.merge(data, stay, on = "FIPS")
data.to_csv('data/cleaned/01_Demographic/opp_atlas_cleaned.csv', index = False)