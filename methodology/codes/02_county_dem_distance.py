# figure out approach to similarity among counties
# needs to incorporate: demographics, options for 6 SDoH variables, or all

import pandas as pd
pd.set_option('display.max_columns', 30)
from sklearn.metrics.pairwise import euclidean_distances

data_dictionary = pd.read_csv('data/data_dictionary.csv')
data = pd.read_csv('data/data_sdoh_scores.csv')

dem_cols = list(data_dictionary[data_dictionary['demographic'] == 1]['column_name'])
print(dem_cols)

print(data[dem_cols].corr())
# should we exclude one of the race columns since they should add up to one? not sure if they actually do

# replace NAs with the mean since euclidean distance doesn't work
data = data.fillna(data.mean())

lol = data[dem_cols].values.tolist()
# distances between rows of X
print(euclidean_distances(lol, lol))

# next, need to look at graphs and maps of how this methodology works out
# how do the similar counties compare in health metrics?