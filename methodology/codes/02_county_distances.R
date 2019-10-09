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

calculate_distance <- function(county_num){
  data$distance <- distancem[,county_num]
  data$flag_county <- ifelse(data$FIPS == head(data$FIPS, county_num), 1, 0)
  return(data)
}

data <- calculate_distance(1)

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

# radar charts in order of county similarity
library(fmsb)
par(mfrow=c(3,2), 
    mar = c(0,0,0,0))
ordered <- data %>% 
  arrange(distance) %>% 
  mutate(rank = row_number(),
         top5 = ifelse(rank == 1, 'county', ifelse(rank <= 6, 1, 0)))

for(i in c(1:nrow(ordered))){
  radar_data <- ordered[i,]
  county <- radar_data$County
  radar_data <- radar_data %>% 
    dplyr::select(starts_with("sdoh_score"))
  colnames(radar_data) <- c("econ","env","edu","food","comm","health")
  # I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  radar_data <- rbind(rep(1,6), rep(0,6), radar_data)
  radarchart(radar_data,
             pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4,
             title = county)
}

# compare health metrics of all and of top 5ish closest counties
data_dictionary %>% 
  filter(outcome == 1) %>% 
  dplyr::select(column_name) %>% 
  pull()
county_outcome <- ordered[1,c("X..Obese")]

ggplot(ordered, aes(x = X..Obese)) +
  geom_density() +
  geom_rug(size = 2, aes(color = top5)) +
  scale_color_manual(values = c("black","red","blue")) +
  geom_vline(xintercept = county_outcome, color = "blue")

# for each county, check top 5 sdoH radar charts and similar outcomes
for(i in c(1:nrow(data))){
  countyi_data <- calculate_distance(i)
  
  ordered <- countyi_data %>% 
    arrange(distance) %>% 
    mutate(rank = row_number(),
           top5 = ifelse(rank == 1, as.character(County), ifelse(rank <= 6, "1", "0")))
  this_county <- ordered %>% filter(rank == 1) %>% select(County) %>% pull()
  
  par(mfrow=c(3,2), 
      mar = c(0,0,0,0))
  
  for(j in c(1:6)){
    radar_data <- ordered[j,]
    county <- radar_data$County
    radar_data <- radar_data %>% 
      dplyr::select(starts_with("sdoh_score"))
    colnames(radar_data) <- c("econ","env","edu","food","comm","health")
    # I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    radar_data <- rbind(rep(1,6), rep(0,6), radar_data)
    radar <- radarchart(radar_data,
                        pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4,
                        title = county)
    print(radar)
  }
  print('radar charts complete')
  
  county_outcome <- ordered[1,c("X..Obese")]
  plt <- ggplot(ordered, aes(x = X..Obese)) +
    geom_density() +
    geom_rug(size = 2, aes(color = top5)) +
    scale_color_manual(values = c("black","red","blue")) +
    geom_vline(xintercept = county_outcome, color = "blue") +
    ggtitle(this_county)
  print(plt) 
}

# Things to check: How close are the top 5 most similar distance scores?
# How similar are health outcomes of the top 5 most similar or similar within a certain distance?
