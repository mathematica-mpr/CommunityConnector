import pandas as pd

datafile = "data/full_data.csv"

data = pd.read_csv(datafile)
print(data.shape)

# preliminary columns to keep
keep_phrases = ['FIPS','County','SEX_BY_AGE','RACE','MEDIAN_HOUSEHOLD_INCOME','PUBLIC_ASSISTANCE_INCOME_Estimate','GINI_INDEX',
'PER_CAPITA_INCOME','MEAN_USUAL_HOURS','HOUSING_UNITS','OCCUPANCY_STATUS','TENURE','BEDROOMS','MEDIAN_GROSS_RENT_AS_A_PERCENTAGE','MEDIAN_VALUE']
keep_vars = []
colnum = 0
fips_colnum = data.shape[1]
for col in data.columns.values:
    # keep everything after the FIPS column
    if col == "FIPS":
        fips_colnum = colnum
    # keep the columns that aren't by two variables to start
    if ("BY" not in col) or (col in ['SEX_BY_AGE_Estimate_Total','SEX_BY_AGE_Estimate_Total_Male','SEX_BY_AGE_Estimate_Total_Female']) or (colnum >= fips_colnum):
        if any(phrase in col for phrase in keep_phrases):
            keep_vars.append(col)
        elif colnum >= fips_colnum:
            keep_vars.append(col)
    colnum += 1

# are there any variables that don't have a ton of data? drop them if low coverage
nrows = data.shape[0]
for var in keep_vars:
    coverage = data[pd.notnull(data[var])].shape[0]/nrows
    # TODO: this is an arbitrary cutoff
    if coverage < 0.8:
        print(f"Dropping {var} due to low coverage ({round(coverage,2)*100}%)")
        keep_vars.remove(var)

# keep the sdoh_scores - these variables will be calculated in the next code
for i in range(1,7):
    keep_vars.append(f"sdoh_score{i}")
print(len(keep_vars))

# export all column names to a data dictionary with the following format:
# column_name|description|demographic|sdoh_raw|outcome|sdoh_score|data_type|used_sdoh_score_1|used_sdoh_score_2|used_sdoh_score_3|used_sdoh_score_4|used_sdoh_score_5|used_sdoh_score_6
pre_dict = pd.read_csv('data/columns.csv')
data_dictionary = pd.DataFrame({'column_name': keep_vars,
# will change the descriptions to something better later
'description': keep_vars})

data_dictionary = pre_dict.merge(data_dictionary, on ='column_name', how = 'inner')
data_dictionary['demographic'] = [int(dtype == "01_Demographic") for dtype in data_dictionary['type']]
data_dictionary['sdoh_raw'] = [int(dtype == "02_SDoH") for dtype in data_dictionary['type']]
data_dictionary['outcome'] = [int(dtype == "03_Outcome") for dtype in data_dictionary['type']]
data_dictionary['sdoh_score'] = [int(dtype == "aggregate") for dtype in data_dictionary['type']]
data_dictionary.drop('type', axis = 1, inplace = True)

def data_type(c):
    if c in ['FIPS','County','State']:
        return 'ID'
    elif c in [f'sdoh_score{i}' for i in range(1,7)]:
        return 'aggregate'
    elif data[c].dtypes in ['float64','int64']:
        if all(data[c] <= 1) or (('%' in c and '95%' not in c) or (c.count("%") > 1)):
            return 'percentage'
        else:
            return 'continuous'
    elif 'Ratio' in c:
        return 'ratio'
    else:
        return 'unknown'

data_dictionary['data_type'] = data_dictionary['column_name'].apply(data_type)
# remove rows with unknown
data_dictionary = data_dictionary[data_dictionary['data_type'] != "unknown"]

# columns for if the column was used in one of the sdoh scores
for i in range(6):
    data_dictionary[f'used_sdoh_{i+1}'] = 0

print(data_dictionary)

# remove additional FIPS columns
for ix, row in data_dictionary.iterrows():
    if row['column_name'] == "FIPS":
        print(ix)
data_dictionary.drop(data_dictionary.index[[38,39]], inplace = True)

# change the description for the SDoH scores
data_dictionary.loc[data_dictionary['column_name'].str.contains('sdoh_score'),'description'] = ['Economic Stability','Neighborhood & Physical Environment',
'Education','Food','Community','Health Coverage']

# Later, will look into variable reduction/impact on outcomes to determine the most important ones

data_dictionary.to_csv('data/data_dictionary.csv')