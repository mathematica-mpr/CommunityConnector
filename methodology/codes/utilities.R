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

# TODO: also add an option to only include relevant SDoH scores or raw inputs
# option to only include non-modifiable SDoH raw metrics
# option to use all SDoH - find the modifiable, impactful ones and then remove them to get a prediction
select_distance_columns <- function(data, data_dictionary, sdoh_scores, sdoh_raw){
  # always use demographics
  cols <- data_dictionary %>% 
    filter(demographic == 1) %>% 
    dplyr::select(column_name) %>% 
    unlist() %>% 
    as.vector()
  
  if(sdoh_scores == 1){
    add_cols <- data_dictionary %>% 
      filter(sdoh_score == 1) %>% 
      dplyr::select(column_name) %>% 
      unlist() %>% 
      as.vector()
    cols <- c(cols, add_cols)
  }
  
  if(sdoh_raw == 1){
    add_cols <- data_dictionary %>% 
      filter(sdoh_raw == 1) %>% 
      dplyr::select(column_name) %>% 
      unlist() %>% 
      as.vector()
    cols <- c(cols, add_cols)
  }
  
  cols <- unique(cols)
  
  use_data <- data %>% 
    dplyr::select(one_of(cols))
  
  use_data <- replace_nas(use_data)
  
  return(use_data)
}

replace_nas <- function(use_data){
  # replace NAs with the mean since euclidean distance doesn't work without it
  use_data <- data.frame(lapply(use_data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
  # TODO: other imputation that isn't supervised?
  # or use random forest if okay with supervised
}

county_distance <- function(use_data, method){
  if(method == "euclidean"){
    distancem <- as.matrix(dist(use_data, method = 'euclidean'))
  } 
  
  # TODO:
  # else if(method == "cosine similarity"){
  #   
  # } else if(method == "rf distance"){
  #   
  # } else if(method == "gbm prediction"){
  #   
  # } else if(method = "rf prediction"){
  # 
  # }
  # TODO: weighted euclidean distance?
  
  return(distancem)
}

select_county <- function(distancem, county_num){
  data$distance <- distancem[,county_num]
  data$flag_county <- ifelse(data$FIPS == head(data$FIPS, county_num), 1, 0)
  return(data)
}