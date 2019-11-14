import pandas as pd
pd.set_option('display.max_colwidth', -1)
pd.set_option('display.max_columns', 30)
import os
import zipfile

raw_output = 'data/raw/'
output = 'data/cleaned/03_Outcome'
# unzip and pull out the 2017 files
# unzip fileS
with zipfile.ZipFile(os.path.join(raw_output, 'cms_prev_data.zip'), 'r') as zip_ref:
    zip_ref.extractall(raw_output)
with zipfile.ZipFile(os.path.join(raw_output, 'cms_spend_data.zip'), 'r') as zip_ref:
    zip_ref.extractall(raw_output)

prev = pd.read_excel(os.path.join(raw_output, "County_Table_Chronic_Conditions_Prevalence_by_Age_2017.xlsx"),
sheet_name = "Beneficiaries 65 Years and Over")

def fix_colnames(data):
    data.columns = data.iloc[4,]
    # for columns 4 - end, need the 3rd row for column names
    data.columns.values[3:] = data.iloc[3,3:]
    data = data.iloc[5:,]
    data['State/County FIPS Code'] = data['State/County FIPS Code'].astype(str)
    return data

prev = fix_colnames(prev)
# not sure why these show up as nan
prev.columns.values[4] = 'Dementia'
prev.columns.values[17] = 'Hepatitis'
cols_use = prev.columns.values
prev.columns.values[3:] = [c + "_pct" for c in prev.columns.values[3:]]
prev = prev[prev['State'].str.contains('Colorado')]
print(prev.shape)
print(prev['State/County FIPS Code'].drop_duplicates().shape)

spend = pd.read_excel(os.path.join(raw_output, "County_Table_Chronic_Conditions_Spending_2017.xlsx"),
sheet_name = "Standardized Spending")
spend = fix_colnames(spend)
spend.columns = cols_use
# replace pct with std_spend
spend.columns = [c.replace("pct","std_spend") for c in spend.columns.values]
spend = spend[spend['State'].str.contains('Colorado')]
print(spend.shape)
print(spend['State/County FIPS Code'].drop_duplicates().shape)

# merge by state/county FIPS code
cms = pd.merge(prev, spend, on = ['State/County FIPS Code'], how = 'outer')

# do this earlier than the merge file because there are issues
cms = cms[cms['State/County FIPS Code'] != "  "]
print(cms.shape)
print(cms['State/County FIPS Code'].drop_duplicates().shape)

cms['FIPS'] = [int(str(fips)[-4:]) for fips in cms['State/County FIPS Code']]
cms = cms[cms['FIPS'] != 999]
print(cms[['FIPS','County_x','County_y']].drop_duplicates())
print(cms['FIPS'].drop_duplicates().shape)

# keep only columns needed
cms = cms[['FIPS','Chronic Kidney Disease_pct','Diabetes_pct','Chronic Kidney Disease_std_spend',
'Diabetes_std_spend']]
# replace * with ""
cms[['Chronic Kidney Disease_pct','Diabetes_pct','Chronic Kidney Disease_std_spend',
'Diabetes_std_spend']] = cms[['Chronic Kidney Disease_pct','Diabetes_pct','Chronic Kidney Disease_std_spend',
'Diabetes_std_spend']].replace("* ","", regex = False)

# export
cms.to_csv(os.path.join(output, "cms_cleaned.csv"), index = False)