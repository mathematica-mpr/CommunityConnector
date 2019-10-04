import pandas as pd

# For now, use the following variables:
# Demographics: Sex, race, median income, GINI, % rural, % female, Life expectancy, infant mortality
# Economic Stability: % unemployed, % free or reduced lunch
# Neighborhood: Housing units, Occupancy status, violent crime rate
# Education: Graduation rate
# Food: Food environment index, % some college, % food insecure
# Community: Hours Worked, mentall unhealthy
# Health Coverage: % Uninsured, PCP Rate, Dentist Rate, MH Rate
# Outcome: % obese, % diabetic, kidney...

# Later, will look into variable reduction/impact on outcomes to determine the most important ones

data = pd.read_csv('data/full_data.csv')
data_dict = pd.read_csv('data/data_dictionary.csv')
pre_rows = data.shape[1]
data = data[data_dict['column_name']]
print(f"# cols dropped: {pre_rows - data.shape[1]}")

print(data['FIPS'])
# which has the most coverage?
print(data[['FIPS','SEX_BY_AGE_Estimate_Total_Male','SEX_BY_AGE_Estimate_Total_Female','% Female']])

demographic_cols = 
# print(data.head())