# this data was pulled from a special request at the county level
# will need to figure out from Keri how she got it and see if it's possible to scrape

import pandas as pd 
import sys
sys.path.insert(1, 'pipeline/codes')
from utilities import pull_population

data = pd.read_csv('data/raw/mort_cause.csv')
data = data[data.Year == 2017]
data.drop(['Notes','County','Year','Year.Code','UCD...ICD.10.113.Cause.List.Code','Deaths','Population',
'Crude.Rate',
'Crude.Rate.Lower.95..Confidence.Interval',
'Crude.Rate.Upper.95..Confidence.Interval','Crude.Rate.Standard.Error','Age.Adjusted.Rate.Lower.95..Confidence.Interval',
'Age.Adjusted.Rate.Upper.95..Confidence.Interval','Age.Adjusted.Rate.Standard.Error'], axis = 1, inplace = True)
data.columns.values[0] = 'FIPS'

# use only Colorado counties
pop = pull_population()
data['FIPS'] = data['FIPS'].astype(int)
data = pd.merge(data, pop, on = "FIPS", how = "right")
data.drop(['population'], axis = 1, inplace = True)
data = data.pivot(index = 'FIPS', columns = 'UCD...ICD.10.113.Cause.List', values = 'Age.Adjusted.Rate')

# low coverage
print(data.isna().sum())