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

            data_columns = pd.DataFrame({'column_name': data.columns.values,
                'type': t
                })

            if count == 0:
                full_data = data
                columns = data_columns
            else:
                full_data = pd.merge(full_data, data, on = 'FIPS', how = 'outer')
                columns = columns.append(data_columns)

            count += 1

print(full_data.shape)

# add sdoh_score1-6 to columns
sdoh_score_names = [f'sdoh_score{i}' for i in range(1,7)]
columns = columns.append(pd.DataFrame({'column_name': sdoh_score_names,
'type': ['aggregate']*6}))

full_data.to_csv('data/full_data.csv')
columns.to_csv('data/columns.csv')