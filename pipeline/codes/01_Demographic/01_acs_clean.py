import pandas as pd
import os

n_drive = "N:/Transfer/KSkvoretz/AHRQ/data/01_Demographic/cleaned"

acs = pd.read_csv(os.path.join(n_drive, "ACS_cleaned.csv"))
acs.columns.values[0] = "location"

acs['FIPS'] = [location[-3:] for location in acs['location']]
print(acs['FIPS'])

acs.to_csv(os.path.join(n_drive, "ACS_cleaned.csv"))