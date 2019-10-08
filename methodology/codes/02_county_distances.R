rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyverse)
# devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr)
library(plotly)
library(ggplot2)
# install.packages("maps")
# install.packages("mapdata")
library(maps)
library(mapdata)
library(stringr)

data_dictionary <- read.csv('../../data/data_dictionary.csv')
data <- read.csv('../../data/data_sdoh_scores.csv')

# TODO:
# test methodologies:
# only use demographic variables
# OR dem + all SDoH scores
# OR dem + some SDoH scores
# OR dem + some SDoH raw metrics
# + weighting?

#####################################################
###########   Demographic variables       ###########
#####################################################

# should we exclude one of the race columns since they should add up to one? not sure if they actually do
dem_cols <- data_dictionary %>% 
  filter(demographic == 1) %>% 
  dplyr::select(column_name) %>% 
  unlist() %>% 
  as.vector()

# need to change some of the names because of column name differences in R and Python
dem_cols[grepl("MEDIAN_HOUSEHOLD", dem_cols)] <- "MEDIAN_HOUSEHOLD_INCOME"
dem_cols[grepl("Life", dem_cols)] <- "Life.Expectancy"
dem_cols[grepl("Female", dem_cols)] <- "X..Female"
dem_cols[grepl("Rural", dem_cols)] <- "X..Rural"
names(data)[grepl("MEDIAN_HOUSEHOLD",names(data))] <- "MEDIAN_HOUSEHOLD_INCOME"

#####################################################
###########  Limit to distance columns    ###########
#####################################################

dist_cols <- dem_cols
dist_cols

use_data <- data %>% 
  dplyr::select(one_of(dist_cols))
# replace NAs with the mean since euclidean distance doesn't work
use_data <- data.frame(lapply(use_data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))

# TODO: euclidean distance or cosine similarity?
head(use_data)
distancem <- as.matrix(dist(use_data, method = 'euclidean'))

#####################################################
###########  Select a county as example  ############
#####################################################

county_num <- 1
data$distance <- distancem[,county_num]
data$flag_county <- ifelse(data$FIPS == head(data$FIPS, county_num), 1, 0)
data$Population_x

###################################################
###### Look at how the methodology works out ######
###################################################

# look at different metrics by distance to the county in question
dem_cols

for(x in c(1:length(dem_cols))){
  for(y in c(1:length(dem_cols))){
    x_var <- dem_cols[x]
    y_var <- dem_cols[y]
    if(x_var != y_var){
      
      county_x <- data %>% filter(flag_county == 1) %>% dplyr::select(x_var) %>% pull()
      county_y <- data %>% filter(flag_county == 1) %>% dplyr::select(y_var) %>% pull()
      
      plt <- ggplot(data = data, aes(x = !!sym(x_var), y = !!sym(y_var), size = distance, color = flag_county)) +
        geom_point() +
        # plot county in question on top
        geom_point(aes(x = county_x, y = county_y), color = 'red', size = 3)
      print(plt)
    }
  }
}

# compare health metrics of all and of top 5ish closest counties



