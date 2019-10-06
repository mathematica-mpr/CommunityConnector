setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)

data_dictionary = read.csv('../../data/data_dictionary.csv')
data = read.csv('../../data/data_sdoh_scores.csv')

dem_cols = data_dictionary %>% 
  filter(demographic == 1) %>% 
  select(column_name)
print(as.list(dem_cols))

data

# should we exclude one of the race columns since they should add up to one? not sure if they actually do
# replace NAs with the mean since euclidean distance doesn't work

# option to use all demographic variables
# + any/all of SDoH scores
# euclidean distance or cosine similarity?

# next, need to look at graphs and maps of how this methodology works out
# how do the similar counties compare in health metrics, demographics, and sdoh?