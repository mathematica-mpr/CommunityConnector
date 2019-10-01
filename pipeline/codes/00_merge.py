# Merge: ACS, RWJF, HCUP
# Soon: BRFSS, USDA after scraping

import os
import pandas as pd

n_drive = "N:/Transfer/KSkvoretz/AHRQ/data"

data_types = ['01_Demographic','02_SDoH','03_Outcome']

count = 0
for t in data_types:
    cleaned_files = os.listdir(os.path.join(n_drive, t, "cleaned"))

    for file in cleaned_files:
        if "csv" in file:
            data = pd.read_csv(os.path.join(n_drive, t, "cleaned", file))
            data['FIPS'] = [str(fips)[-3:].zfill(3) for fips in data['FIPS']]
            print(data.shape)
            if count == 0:
                full_data = data
            else:
                full_data = pd.merge(full_data, data, on = 'FIPS', how = 'outer')

            count += 1

print(full_data.shape)

full_data.to_csv(os.path.join(n_drive, 'full_data.csv'))