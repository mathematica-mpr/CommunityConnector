list.of.packages <- c("tidyverse","plotly","ggplot2","maps","mapdata","stringr","fmsb","randomForest",
                      "caret","glmnet","glmnetUtils","data.table","gbm","MLmetrics","distances",
                      "psycho","OpenRepGrid")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

select_distance_columns <- function(data, data_dictionary, sdoh_scores, sdoh_raw, outcome, dem = TRUE){
  
  # list of columns that will be used
  cols <- c()
  
  # default to always use demographic columns
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
  
  # add outcome column
  cols <- c(outcome, unique(cols))
  
  # limit data to the columns we are using
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

# find modifiable variables - these ones will be replaced with the worst possible outcome in the data, assuming the county did nothing
replace_modifiable <- function(coefs_df, data_dictionary, use_data){
  replace_cols <- coefs_df %>% 
    merge(data_dictionary, by.x = "name", by.y = "column_name", all.x = TRUE) %>% 
    filter(modifiable == 1) %>% 
    dplyr::select(name, modifiable, higher_better)
  
  # columns that are better lower, we replace with the max - this is the worst possible outcome
  replace_cols_max <- replace_cols %>% filter(higher_better == 0) %>% dplyr::select(name) %>% pull() %>% as.character()
  # columns that are better higher, we replace with the min - this is the worst possible outcome
  replace_cols_min <- replace_cols %>% filter(higher_better == 1) %>% dplyr::select(name) %>% pull() %>% as.character()
  
  set_max <- function(x, na.rm = TRUE){
    max <- max(x, na.rm)
    return(max)
  }
  set_min <- function(x, na.rm = TRUE){
    min <- min(x, na.rm)
    return(min)
  }
  
  # replace modifiable variables in data
  use_data <- use_data %>% 
    mutate_at(replace_cols_max, set_max) %>% 
    mutate_at(replace_cols_min, set_min) 
  return(use_data)
}

# TODO: ideally if we had more data, we would split the data into train and test sets to build the models, but this is just a use case
county_distance <- function(use_data, fips, data_dictionary, method, outcome, remove_modifiable, model_params = NA, show_deets = FALSE){
  mse <- NA
  mtry <- NA
  alpha <- NA
  min_lambda <- NA
  formula <- as.formula(paste(outcome,"~."))
  metric <- "RMSE"
  n_rows <- nrow(model_params)
  
  set.seed(1234)
  
  # unweighted euclidean distance using all columns besides the outcome - most basic methodology
  if(method == "euclidean"){
    use_data <- replace_nas_mean(use_data)
    distancem <- as.matrix(dist(use_data %>% 
                                  dplyr::select(-!!rlang::sym(outcome)),
                                method = 'euclidean'))
  # Random forest methodologies include finding the difference between predictions or using the RF proximity matrix
  } else if(grepl("rf",method)){
    meth <- "rf"
    use_data <- replace_nas_rf(use_data, outcome)
    
    # If we don't have model parameters, tune the model
    if(length(n_rows) == 0){
      # cross-validation on mtry
      # TODO: could change back to repeatedcv later but changing for sake of time
      # trControl <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
      trControl <- trainControl(method="cv", number=10, search="grid")
      tunegrid <- expand.grid(.mtry=c(ceiling(sqrt(ncol(use_data))):ncol(use_data)-1))
      rf <- train(formula, data=use_data, method=meth, metric=metric, tuneGrid=tunegrid, trControl=trControl)
      mtry <- rf$bestTune$mtry 
    # Otherwise, use the model parameters we saved
    } else {
      mtry <- model_params$mtry
    }

    rf <- randomForest(formula, data=use_data, method=meth, metric=metric, mtry = mtry,
                       keep.forest = TRUE, importance = TRUE, proximity = TRUE)
    
    if(show_deets){
      importance(rf)
      varImp(rf, scale = FALSE)
      varImpPlot(rf, n.var=min(20,ncol(use_data)-1), sort = TRUE)
    }
    
    if(remove_modifiable){
      # find modifiable variables - these ones will be replaced with the worst possible outcome in the data to assume the county did nothing
      coefs_df <- as.data.frame(colnames(use_data))
      colnames(coefs_df)[1] <- "name"
      use_data <- replace_modifiable(coefs_df, data_dictionary, use_data)
    }
    
    pred <- predict(rf, new_data = use_data)
    
    if(method == "rf proximity"){
      distancem <- rf$proximity
    } else {
      distancem <- abs(outer(pred, pred, '-'))
    }
  
  # Lasso methodology options: distance between predictions or euclidean distance using coefficients as weights
  } else if(grepl("lasso",method)){
    use_data <- replace_nas_rf(use_data, outcome)
    
    # cross validation for alpha and lambda if we don't have it already
    if(length(n_rows) == 0){
      cva <- glmnetUtils::cva.glmnet(formula, data = use_data)
      
      cv.glmnet.dt <- data.table::data.table()
      for (i in c(1:length(cva$alpha))){
        glmnet.model <- cva$modlist[[i]]
        min.mse <-  min(glmnet.model$cvm)
        min.lambda <- glmnet.model$lambda.min
        alpha.value <- cva$alpha[i]
        new.cv.glmnet.dt <- data.table::data.table(alpha=alpha.value,min_mse=min.mse,min_lambda=min.lambda)
        cv.glmnet.dt <- rbind(cv.glmnet.dt,new.cv.glmnet.dt)
      }
      
      best.params <- cv.glmnet.dt[which.min(cv.glmnet.dt$min_mse)]
    # Otherwise, use the model parameters we saved
    } else {
      best.params <- model_params
    }
    
    alpha <- best.params$alpha
    min_lambda <- best.params$min_lambda
    
    lasso <- glmnet::glmnet(as.matrix(use_data[,!names(use_data) %in% outcome]),
                            as.matrix(use_data[,names(use_data) %in% outcome]),
                            alpha = alpha,
                            lambda = min_lambda)
    coefs <- coef(lasso)
    coefs_df <- data.frame(name = coefs@Dimnames[[1]][coefs@i + 1], coefficient = coefs@x)
    print(coefs_df)
    
    if(remove_modifiable){
      # find modifiable variables - these ones will be replaced with the worst possible outcome in the data
      use_data <- replace_modifiable(coefs_df, data_dictionary, use_data)
      
    }
    
    if(method == "lasso"){
      pred <- as.numeric(predict(lasso, newx = as.matrix(use_data[,!names(use_data) %in% outcome]), type = "response"))
      distancem <- abs(outer(pred, pred, '-'))  
    } else {
      
      # use only the coefficients of the nonmodifiable variables
      weights <- coefs_df %>% 
        merge(data_dictionary, by.x = "name", by.y = "column_name") %>% 
        dplyr::select(name, coefficient, demographic, sdoh_raw, modifiable) %>% 
        filter(is.na(modifiable)) %>% 
        mutate(abs_coefficient = abs(coefficient))
      
      # one option is to only use demographics coefficients as weights
      if(method == "lasso euclidean dem"){
        weights <- weights %>% 
          dplyr::filter(demographic == 1)
      }
      
      var_names <- weights %>% 
        dplyr::select(name) %>% 
        pull() %>% 
        as.character()
      
      weights <- weights %>% 
        dplyr::select(abs_coefficient) %>% 
        pull() %>% 
        as.numeric()
      
      if(length(var_names) > 1){
        distancem <- distances(use_data[,var_names], id_variable = fips, weights = weights)
      } else {
        distancem <- abs(outer(use_data[,var_names], use_data[,var_names], '-')) 
      }
    }
    
  } 
  
  if(!grepl("euclidean",method)){
    mse <- MSE(pred, use_data[,names(use_data) %in% outcome])
  }
  
  return(list(distancem, mse, mtry, alpha, min_lambda, coefs_df))
}

# select a single county's distances to other counties to evaluate
select_county <- function(data, distancem, county_num){
  data$distance <- distancem[,county_num]
  data$flag_county <- ifelse(data$fips == tail(head(data$fips, county_num),1), 1, 0)
  return(data)
}

# A number of ways to evaluate the methodology
evaluate_methodology <- function(data, use_outcome){
  par(mfrow=c(3,2), 
      mar = c(0, 0,0,0))
  # select the closest 5 counties
  ordered <- data %>% 
    arrange(distance) %>% 
    mutate(rank = row_number(),
           top5 = ifelse(rank == 1, 'county', ifelse(rank <= 6, 1, 0))) %>% 
    filter(!is.na(!!rlang::sym(use_outcome)))
  
  # my county's outcome & fips code
  county_outcome <- ordered[1,use_outcome]
  this_county <- ordered %>% filter(rank == 1) 
  this_county <- this_county$fips
  
  # rug plot of outcomes of my county vs. top 4 vs. all counties
  # plt <- ggplot(ordered, aes(x = !!rlang::sym(use_outcome))) +
  #   geom_density() +
  #   geom_rug(size = 2, aes(color = top5)) +
  #   scale_color_manual(values = c("black","red","blue")) +
  #   geom_vline(xintercept = county_outcome, color = "blue") +
  #   ggtitle(this_county)
  # print(plt) 
  
  top5 <- ordered[ordered$rank <= 6, ]
  
  # SD of all outcomes
  sd <- sd(ordered[,use_outcome])
  # SD of top 5 outcomes should be reduced
  sd_top5 <- sd(top5[, use_outcome])
  # by how much?
  pct_reduced_sd <- 100*abs(sd-sd_top5)/sd
  
  # median of top 5 outcomes
  med_top5 <- median(top5[, use_outcome])
  # how different is the median from my county?
  pct_diff_from_county_med <- 100*abs(med_top5-this_county)/this_county
  
  # standard deviation of other key variables of top 5 counties
  sds <- sapply(top5[,c("median_income","frac_coll_plus2010",
                 "pct_physically_inactive","budget_health_svcs",
                 "pct_food_insecure","pct_limited_access",
                 "pct_with_access")], sd, na.rm = TRUE) 
  names(sds) <- paste0("sd_",names(sds))
  sds <- c(sds, "pct_diff_from_county_med" = pct_diff_from_county_med,
           "pct_reduced_sd" = pct_reduced_sd)
  
  return(as.data.frame(sds))
}

implement_methodology <- function(row, outcomes, data, data_dictionary, all_outcome_params = NA, num_counties = NA){
  
  # Define variables from opts dataframe
  use_sdoh_scores <- as.numeric(row$use_sdoh_scores)
  use_sdoh_raw <- as.numeric(row$use_sdoh_raw)
  use_dems <- as.numeric(row$use_dems)
  remove_modifiable <- as.numeric(row$remove_modifiable)
  methodology <- as.character(row$methodology)
  meth_num <- as.numeric(row$meth_num)
  
  # warn to normalize variables
  if(method %in% c("lasso euclidean all","lasso euclidean dem")){
    message("All variables must be normalized. Please fix if not.")
  }
  
  # empty list that will be filled in with distance matrices
  distance_list <- list()
  
  start_time <- Sys.time()
  print(methodology)
  print(start_time)
  
  # Loop through all outcomes
  n <- 1
  for(use_outcome in outcomes){
    print(paste("Outcome:", use_outcome))
    
    # only keep data where outcome exists
    orig_data <- data %>% 
      filter(!is.na(!!rlang::sym(use_outcome)))
    
    # Select variables to match on, limit data to these variables, and replace NAs
    use_data <- select_distance_columns(data = orig_data, data_dictionary = data_dictionary,
                                        sdoh_scores = use_sdoh_scores, sdoh_raw = use_sdoh_raw,
                                        outcome = use_outcome, dem = use_dems)
    
    # this chunk makes the code more efficient so we don't have to tune the same model multiple times for the same outcome/variables
    # instead, we can input tuning parameters stored in a dataframe from a past best model
    n_rows <- nrow(all_outcome_params)
    model_params <- NA
    if(length(n_rows) != 0){
      out <- use_outcome
      model_params <- all_outcome_params %>% 
        filter(use_outcome == out)
    }
    
    # Get distance matrix using methodology specified
    dist_results <- county_distance(use_data, orig_data$fips, data_dictionary, methodology, use_outcome, remove_modifiable, model_params)
    # Function outputs:
    # distance matrix
    distancem <- dist_results[1][[1]]
    # mse of model
    mse <- dist_results[2][[1]]
    # best tuning parameters to save in case we need to re-run
    mtry <- dist_results[3][[1]]
    alpha <- dist_results[4][[1]]
    min_lambda <- dist_results[5][[1]]
    # model coefficients
    coefs_df <- dist_results[6][[1]]
    
    ## Evaluate the methodology:
    # Look at the radar charts in order of county similarity
    # How similar are health outcomes of the top 5 most similar or similar within a certain distance?
    # They should have a median close to the county in question
    
    # use all counties to evaluate if we didn't specify how many to use
    if(is.na(num_counties)){
      n_counties <- dim(distancem)[1]
    } else {
      n_counties <- num_counties
    }
    
    # Loop through counties to get spread metrics for each county's top 5 most similar
    for(county_num in c(1:n_counties)){
      data <- select_county(orig_data, distancem, county_num)
      
      results <- evaluate_methodology(data, use_outcome)
      results$metric <- rownames(results)
      results_df <- results %>% 
        spread(metric, sds) %>% 
        mutate("mse" = mse,
               "county_num" = county_num,
               "use_outcome" = use_outcome,
               "methodology" = methodology,
               "meth_num" = meth_num,
               "mtry" = mtry,
               "alpha" = alpha,
               "min_lambda" = min_lambda)
      
      # append results of all counties
      if(use_outcome == outcomes[1]){
        full_results <- results_df
      } else {
        full_results <- full_results %>% 
          rbind(results_df)
      }
      
    }
    
    # append coefficients of all models
    if(n == 1){
      coefs_all <- coefs_df
    } else {
      coefs_all <- coefs_all %>% 
        merge(coefs_df, by = "name", all.x = TRUE, all.y = TRUE)
    }
    names(coefs_all)[n+1] <- use_outcome
    distance_list[[n]] <- distancem
    n <- n+1
    
  }
  
  end_time <- Sys.time()
  print(paste0("Time elapsed: ", end_time - start_time))
  start_time <- end_time
  
  return(list(full_results,distance_list, coefs_all))
}