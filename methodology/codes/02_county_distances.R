setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)

data_dictionary <- read.csv('../../data/data_dictionary.csv')
data <- read.csv('../../data/data_sdoh_scores.csv')

# option to use all demographic variables
# + any/all of SDoH scores
# should we exclude one of the race columns since they should add up to one? not sure if they actually do
dem_cols <- data_dictionary %>% 
  filter(demographic == 1) %>% 
  select(column_name) %>% 
  unlist() %>% 
  as.vector()

# need to change some of the names because of column name differences in R and Python
dem_cols[grepl("MEDIAN_HOUSEHOLD", dem_cols)] <- "MEDIAN_HOUSEHOLD_INCOME"
dem_cols[grepl("Life", dem_cols)] <- "Life.Expectancy"
dem_cols[grepl("Female", dem_cols)] <- "X..Female"
dem_cols[grepl("Rural", dem_cols)] <- "X..Rural"
names(data)[grepl("MEDIAN_HOUSEHOLD",names(data))] <- "MEDIAN_HOUSEHOLD_INCOME"

use_data <- data %>% 
  select(one_of(dem_cols))
# replace NAs with the mean since euclidean distance doesn't work
use_data <- data.frame(lapply(use_data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

# TODO: euclidean distance or cosine similarity?
head(use_data)
distancem <- as.matrix(dist(use_data, method = 'euclidean'))

county_num = 1
distancem[,county_num]

# next, need to look at graphs and maps of how this methodology works out
# how do the similar counties compare in health metrics, demographics, and sdoh?