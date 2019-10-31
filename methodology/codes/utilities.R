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
library(glmnet)
library(gbm)
library(MLmetrics)

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

# TODO: option to remove modifiable, relevant SDoH scores or inputs to get a prediction. Similarity score = distance between predictions
# TODO: ideally if we had more data, we would split the data into train and test sets to build the models
county_distance <- function(use_data, method, outcome, show_deets = FALSE){
  mse <- NA
  formula <- as.formula(paste(outcome,"~."))
  metric <- "RMSE"
  
  if(method == "euclidean"){
    use_data <- replace_nas_mean(use_data)
    distancem <- as.matrix(dist(use_data %>% 
                                  dplyr::select(-!!rlang::sym(outcome)),
                                method = 'euclidean'))
  } else if(grepl("rf",method)){
    meth <- "rf"
    use_data <- replace_nas_rf(use_data, outcome)
    set.seed(1234)
    
    # cross-validation on mtry
    trControl <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
    tunegrid <- expand.grid(.mtry=c(sqrt(ncol(use_data)):ncol(use_data)))
    rf <- train(formula, data=use_data, method=meth, metric=metric, tuneGrid=tunegrid, trControl=trControl)
    best_mtry <- rf$bestTune$mtry

    rf <- randomForest(formula, data=use_data, method=meth, metric=metric, mtry = best_mtry,
                       keep.forest = TRUE, importance = TRUE, proximity = TRUE)
    
    if(show_deets){
      importance(rf)
      varImp(rf, scale = FALSE)
      varImpPlot(rf, n.var=min(20,ncol(use_data)-1), sort = TRUE)
    }
    pred <- predict(rf)
    
    if(method == "rf proximity"){
      distancem <- rf$proximity
    } else {
      distancem <- abs(outer(pred, pred, '-'))
    }
    
  } else if(method == "lasso"){
    # TODO: other way to replace missing?
    use_data <- replace_nas_rf(use_data, outcome)
    lasso <- glmnet::glmnet(as.matrix(use_data[,!names(use_data) %in% outcome]),
                            as.matrix(use_data[,names(use_data) %in% outcome]),
                            alpha = 1)
    # TODO: important coefficients
    # print(predict(lasso, type = 'coefficients'))[c(1:6)]
    # TODO: cross validation of s or lambda
    pred <- as.numeric(predict(lasso, newx = as.matrix(use_data[,!names(use_data) %in% outcome]),
                    s = 0.01, type = "response"))
    distancem <- abs(outer(pred, pred, '-'))
  } else if(method == "gbm prediction"){
    # https://www.datacamp.com/community/tutorials/decision-trees-R
    # boosting? useful when you have a lot of data and expect the decision trees to be very complex
    gbmod <- gbm(formula,
                 data = use_data, distribution = 'gaussian',
                 # TODO: cross validation to choose
                 shrinkage = 0.01, interaction.depth = 4)
    gbmod.summ <- summary(gbmod, cBars = 25)
    # print(gbmod.summ)
    # TODO: CV for n.trees?
    pred <- predict(gbmod, newdata = use_data, n.trees = 100)
    distancem <- abs(outer(pred, pred, '-'))
  }
  
  if(method != "euclidean"){
    mse <- MSE(pred, use_data[,names(use_data) %in% outcome])
  }
  
  return(list(distancem, mse))
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
  
  # for(i in c(1:6)){
  #   radar_data <- data[i,]
  #   county <- radar_data$fips
  #   radar_data <- radar_data %>% 
  #     dplyr::select(starts_with("sdoh_score"))
  #   colnames(radar_data) <- c("econ","env","edu","food","comm","health")
  #   # I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
  #   radar_data <- rbind(rep(1,6), rep(0,6), radar_data)
  #   radarchart(radar_data,
  #              pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4)
  # }
  
  county_outcome <- ordered[1,use_outcome]
  
  this_county <- ordered %>% filter(rank == 1) 
  this_county <- this_county$fips
  
  # plt <- ggplot(ordered, aes(x = !!rlang::sym(use_outcome))) +
  #   geom_density() +
  #   geom_rug(size = 2, aes(color = top5)) +
  #   scale_color_manual(values = c("black","red","blue")) +
  #   geom_vline(xintercept = county_outcome, color = "blue") +
  #   ggtitle(this_county)
  # print(plt) 
  
  sd <- sd(ordered[,use_outcome])
  sd_top5 <- sd(ordered[ordered$rank <= 6, use_outcome])
  med_top5 <- median(ordered[ordered$rank <= 6, use_outcome])
  pct_diff_from_county_med <- 100*abs(med_top5-this_county)/this_county
  pct_reduced_sd <- 100*abs(sd-sd_top5)/sd
  return(list(pct_diff_from_county_med, pct_reduced_sd))
}

implement_methodology <- function(row, outcomes, data, data_dictionary, num_counties = 1){
  
  # Define variables from opts dataframe
  use_sdoh_scores <- as.numeric(row[1])
  use_sdoh_raw <- as.numeric(row[2])
  use_dems <- as.numeric(row[3])
  methodology <- row[4]
  meth_num <- as.numeric(row[5])
  
  print(methodology)
  
  # Loop through all outcomes
  for(use_outcome in outcomes){
    print(paste("Outcome:", use_outcome))
    
    orig_data <- data %>% 
      filter(!is.na(!!rlang::sym(use_outcome)))
    
    # Select variables to match on, limit data to these variables, and replace NAs
    use_data <- select_distance_columns(data = orig_data, data_dictionary = data_dictionary,
                                        sdoh_scores = use_sdoh_scores, sdoh_raw = use_sdoh_raw,
                                        outcome = use_outcome, dem = use_dems)
    
    # Get distance matrix using methodology specified
    dist_results <- county_distance(use_data, methodology, use_outcome)
    distancem <- dist_results[1][[1]]
    mse <- dist_results[2][[1]]
    
    # num_counties <- dim(distancem)[1]
    
    # Loop through counties
    for(county_num in c(1:num_counties)){
      
      data <- select_county(orig_data, distancem, county_num)
      
      ## Evaluate the methodology:
      # Look at the radar charts in order of county similarity
      # How similar are health outcomes of the top 5 most similar or similar within a certain distance?
      # They should have a median close to the county in question
      results <- evaluate_methodology(data, use_outcome)
      results <- c(county_num, use_outcome, methodology, meth_num, results)
      results_df = as.data.frame(matrix(unlist(results), nrow = 1))
      colnames(results_df) <- c('county','outcome','methodology','meth_num','pct_diff_from_county_med','pct_reduced_sd')
      num_vars <- c('county','meth_num','pct_diff_from_county_med','pct_reduced_sd')
      results_df[,num_vars] <- as.numeric(as.character(unlist(results_df[,num_vars])))
      results_df$mse <- mse
      
      # append results of all counties
      if(use_outcome == outcomes[1]){
        full_results <- results_df
      } else {
        full_results <- full_results %>% 
          rbind(results_df)
      }
      
    }
  }
  
  print(full_results)
  return(full_results)
}
