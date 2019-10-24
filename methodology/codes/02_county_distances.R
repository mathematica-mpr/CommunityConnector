rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('utilities.R')

# TODO: also parse down the demographic variables?
data_dictionary <- read.csv('../../data/final_data_dictionary.csv')
data <- read.csv('../../data/final_data.csv')

## Select variables to match on, limit data to these variables, and replace NAs
use_data <- select_distance_columns(data, data_dictionary, 1, 1)

## Get distance matrix using methodology specified
distancem <- county_distance(use_data, 'euclidean')

## Select a county as an example
distancec <- select_county(1)

## Evaluate the methodology

# look at different metrics by distance to the county in question
# Things to check: How close are the top 5 most similar distance scores? They should have a median close to the county in question
# How similar are health outcomes of the top 5 most similar or similar within a certain distance?

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
