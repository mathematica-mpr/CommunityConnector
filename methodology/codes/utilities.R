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
library(fmsb)
library(randomForest)
library(caret)

select_distance_columns <- function(data, data_dictionary, sdoh_scores, sdoh_raw, outcome, dem = TRUE){
  
  cols <- c()
  
  if(dem){
    add_cols <- data_dictionary %>% 
      filter(demographic == 1) %>% 
      dplyr::select(column_name) %>% 
      unlist() %>% 
      as.vector()
    cols <- c(cols, add_cols)
  }
  
  if(sdoh_scores){
    add_cols <- data_dictionary %>% 
      filter(sdoh_score == 1) %>% 
      dplyr::select(column_name) %>% 
      unlist() %>% 
      as.vector()
    cols <- c(cols, add_cols)
  }
  
  if(sdoh_raw){
    add_cols <- data_dictionary %>% 
      filter(sdoh_raw == 1) %>% 
      dplyr::select(column_name) %>% 
      unlist() %>% 
      as.vector()
    cols <- c(cols, add_cols)
  }
  
  cols <- c(outcome, unique(cols))
  
  use_data <- data %>% 
    dplyr::select(one_of(cols))
  
  return(use_data)
}

replace_nas_mean <- function(use_data){
  # replace NAs with the mean since euclidean distance doesn't work without it
  use_data <- data.frame(lapply(use_data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
  return(use_data)
  # TODO: other type of imputation that isn't supervised?
}

replace_nas_rf <- function(use_data, outcome){
  if(nrow(use_data[rowSums(is.na(use_data))>0,]) > 0){
    use_data <- rfImpute(as.formula(paste(outcome,"~.")), use_data) 
  }
  return(use_data)
}

get.elbow.points.indices <- function(x, y, threshold) {
  d1 <- diff(y) / diff(x) # first derivative
  d2 <- diff(d1) / diff(x[-1]) # second derivative
  indices <- which(abs(d2) > threshold)  
  return(indices)
}

pick_mtry <- function(outcome, data){
  oob.err = double(ncol(data)-1)
  test.err = double(ncol(data)-1)
  
  data <- replace_nas_rf(data, outcome)
  
  # to get decorrelated trees, test number of variables to pull from
  for(mtry in 1:ncol(data)-1){
    rf <- randomForest(as.formula(paste(outcome,"~.")),
                       data = data,
                       method = 'rf',
                       metric = 'RMSE',
                       trControl = trControl, 
                       keep.forest = TRUE,
                       importance = TRUE,
                       mtry = mtry)
    oob.err[mtry] = rf$mse[350]
    pred = predict(rf)
    test.err[mtry] = mean((data[,outcome]-pred)^2)
  }
  par(mfrow=c(1,1), 
      mar = c(2, 2, 2, 2))
  matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
  legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))
  lines(predict(loess(test.err ~ c(1:mtry))), col = 'green')
  
  # try cutoffs to get less than n elbowpoints
  cutoff <- 1
  # placeholder for elbows
  elbows <- c(1:100)
  last_length <- length(elbows)
  num_same <- 0
  while(((last_length > 8) | (last_length == 0)) & (num_same <= 5) & (cutoff > 0.05)){
    elbows <- get.elbow.points.indices(1:mtry, test.err, cutoff/10)
    if(length(elbows) == 0){
      cutoff <- cutoff - 0.1
    } else if(length(elbows) == last_length){
      cutoff <- cutoff + 5
      num_same <- num_same + 1
    } else {
      cutoff <- cutoff + 1
    }
    last_length <- length(elbows)
  }
  
  if(length(elbows) == 0){
    elbows <- c("None")
  }
  
  return(elbows)
}

# TODO: option to remove modifiable, relevant SDoH scores or inputs to get a prediction. Similarity score = distance between predictions
county_distance <- function(use_data, method, outcome, mtry = NULL){
  if(method == "euclidean"){
    use_data <- replace_nas_mean(use_data)
    distancem <- as.matrix(dist(use_data %>% 
                                  select(-!!rlang::sym(outcome)),
                                method = 'euclidean'))
    # TODO:
    # else if(method == "cosine similarity"){
    #   
  } else if(grepl("rf",method)){
    use_data <- replace_nas_rf(use_data, outcome)
    set.seed(1234)
    # TODO: optimize any parameters here? - tried optimizing mtry
    trControl <- trainControl(method = 'cv',
                                            number = 10,
                                            search = 'grid')
    rf <- randomForest(as.formula(paste(outcome,"~.")),
                       data = use_data,
                       method = 'rf',
                       metric = 'RMSE',
                       trControl = trControl, 
                       keep.forest = TRUE,
                       importance = TRUE,
                       proximity = TRUE)
    # importance(rf)
    # varImp(rf, scale = FALSE)
    varImpPlot(rf, n.var=min(20,ncol(use_data)-1), sort = TRUE)
    
    if(method == "rf proximity"){
      distancem <- rf$proximity
    } 
    # else {
    # pred <- predict(rf)
    # }
    
  }
  # else if(method == "gbm prediction"){
    # # https://www.datacamp.com/community/tutorials/decision-trees-R
    # # boosting? useful when you have a lot of data and expect the decision trees to be very complex
    # library(gbm)
    # gbmod <- gbm(pct_obese~., data = data, distribution = 'gaussian',
    #              shrinkage = 0.01, interaction.depth = 4)
    # gbmod.summ <- summary(gbmod, cBars = 25)
    # View(gbmod.summ)
  # TODO: weighted euclidean distance?
  
  return(distancem)
}

select_county <- function(data, distancem, county_num){
  data$distance <- distancem[,county_num]
  data$flag_county <- ifelse(data$fips == head(data$fips, county_num), 1, 0)
  return(data)
}

evaluate_methodology <- function(data, use_outcome){
  par(mfrow=c(3,2), 
      mar = c(0, 0,0,0))
  ordered <- data %>% 
    arrange(distance) %>% 
    mutate(rank = row_number(),
           top5 = ifelse(rank == 1, 'county', ifelse(rank <= 6, 1, 0))) %>% 
    filter(!is.na(!!rlang::sym(use_outcome)))
  
  for(i in c(1:6)){
    radar_data <- data[i,]
    county <- radar_data$fips
    radar_data <- radar_data %>% 
      dplyr::select(starts_with("sdoh_score"))
    colnames(radar_data) <- c("econ","env","edu","food","comm","health")
    # I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
    radar_data <- rbind(rep(1,6), rep(0,6), radar_data)
    radarchart(radar_data,
               pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4)
  }
  
  county_outcome <- ordered[1,use_outcome]
  
  this_county <- ordered %>% filter(rank == 1) %>% select(fips) %>% pull()
  plt <- ggplot(ordered, aes(x = !!rlang::sym(use_outcome))) +
    geom_density() +
    geom_rug(size = 2, aes(color = top5)) +
    scale_color_manual(values = c("black","red","blue")) +
    geom_vline(xintercept = county_outcome, color = "blue") +
    ggtitle(this_county)
  print(plt) 
  
  sd <- sd(ordered[,use_outcome])
  sd_top5 <- sd(ordered[ordered$rank <= 6, use_outcome])
  med_top5 <- median(ordered[ordered$rank <= 6, use_outcome])
  pct_diff_from_county_med <- 100*abs(med_top5-this_county)/this_county
  pct_reduced_sd <- 100*abs(sd-sd_top5)/sd
  return(list(pct_diff_from_county_med, pct_reduced_sd))
}
