# Merge: ACS, RWJF, HCUP
# Soon: BRFSS, USDA after scraping

import os
import pandas as pd

cleaned_drive = "data/cleaned"

data_types = ['01_Demographic','02_SDoH','03_Outcome']

count = 0
for t in data_types:
    cleaned_files = os.listdir(os.path.join(cleaned_drive, t))

    for file in cleaned_files:
        if "csv" in file:
            data = pd.read_csv(os.path.join(cleaned_drive, t, file))
            print(file)
            print(data.shape)

            data['FIPS'] = [int(str(fips)[-3:]) for fips in data['FIPS']]
            # make sure the file is unique by FIPS
            print(data['FIPS'].drop_duplicates().shape)
            data_columns = pd.DataFrame({'column_name': data.columns.values,
                'type': t
                })

            if count == 0:
                full_data = data
                columns = data_columns
            else:
                full_data = pd.merge(full_data, data, on = 'FIPS', how = 'outer')
                columns = columns.append(data_columns)
            print(f"Completed {file}")
            print(full_data.shape)
            print('')

            count += 1

data['FIPS'] = [str(fips).zfill(3) for fips in data['FIPS']]

print(full_data.shape)

# add sdoh_score1-6 to columns
sdoh_score_names = [f'sdoh_score{i}' for i in range(1,7)]
columns = columns.append(pd.DataFrame({'column_name': sdoh_score_names,
'type': ['aggregate']*6}))

full_data.to_csv('data/full_data.csv')
columns.to_csv('data/columns.csv')