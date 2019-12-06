import os
import pandas as pd
import numpy as np
import regex as re

input = 'data/raw'

hcup_files = os.listdir(input)
hcup_files = [file for file in hcup_files if "HCUP" in file]
hcup_files = [file for file in hcup_files if "selections" not in file]
print(hcup_files)

# parameters used for each dataframe
selections_df = pd.read_csv(os.path.join(input, "HCUP_selections.csv"))

# pandas isn't working
def getCSV(filePath, sep=","):
    with open(filePath, "r") as f:
        return [l.split(sep) for l in f]

# Turn each dataframe into long and concatenate all
for i in range(len(hcup_files)):
    # data = pd.read_csv(os.path.join(input, f"HCUP_{i}.csv"))
    data = pd.DataFrame(getCSV(os.path.join(input, f"HCUP_{i}.csv")))
    # remove the junk above the data
    data = data.iloc[6:]
    print(data)
    # use the first row as the column names
    data.columns = data.iloc[0]
    data.columns.values[0:2] = ["County","FIPS"]
    data = data.iloc[7:]
    data = data.iloc[1:]

    # assign data with the parameter selections, to make column names later
    data['Analysis Selection'] = selections_df.iloc[i]['Analysis Selection']
    data['Classification'] = selections_df.iloc[i]['Classification']
    data['Diagnosis'] = selections_df.iloc[i]['Diagnosis']
    data['num'] = i

    # wide to long data
    # key columns: county, FIPS, Analysis Selection, Classification, Diagnosis
    data = pd.melt(data, id_vars = ['County','FIPS','Analysis Selection','Classification','Diagnosis','num'],
    var_name='metric', value_name = 'value')

    # stack all HCUP data frames
    if i == 0:
        full_data = data
    else:
        full_data = full_data.append(data)

# drop all rows with * or blank metric
pre_rows = full_data.shape[0]
full_data = full_data[full_data['value'] != "*"]
full_data = full_data[pd.notnull(full_data['value'])]
full_data = full_data[pd.notnull(full_data['County'])]
full_data = full_data[pd.notnull(full_data['FIPS'])]
print(f"{pre_rows - full_data.shape[0]} rows dropped")

# concatenate analysis selection, classification, and diagnosis in some way with shortened metric names
# these will eventually be column names
def change_metric(row):
    if row['metric'] == "Total number of discharges":
        return 'discharge_total'
    elif row['metric'] == "Rate of discharges per 100,000 population":
        return 'discharge_p100'
    else:
        return 'discharge_adj'

full_data['new_metric'] = full_data.apply(change_metric, axis = 1)

full_data['column'] = [re.sub(r'[0-9]+ ', '', diag) for diag in full_data['Diagnosis']]
full_data['column'] = [diag.replace("& ","").replace(" ","_").replace("for_","").replace("/","") for diag in full_data['column']]
full_data['column'] = full_data['column'] + "_" + full_data['new_metric']

full_data = full_data[['FIPS','column','value']]

# long to wide data - one row for every FIPS code 
full_data = full_data.pivot(index = 'FIPS', columns = 'column', values = 'value')
print(full_data.shape)
print(full_data.head())

# output data
full_data.to_csv('data/cleaned/HCUP_cleaned.csv')