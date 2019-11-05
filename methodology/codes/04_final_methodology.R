rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('utilities.R')

data_dictionary <- read.csv('../../data/final_data_dictionary.csv')
data <- read.csv('../../data/final_data.csv')

outcomes <- data_dictionary %>% 
  filter(outcome == 1) %>% 
  dplyr::select(column_name) %>% 
  mutate(column_name = as.character(column_name)) %>% 
  pull()

# methods = c("rf proximity","euclidean","rf prediction","lasso","lasso euclidean all","lasso euclidean dem")
opts <- data.frame("use_sdoh_scores" = 0,
                   "use_sdoh_raw" = 1,
                   "use_dems" =  1,
                   "remove_modifiable" = 0,
                   "methodology" = "lasso euclidean all",
                   "meth_num" = 1)

upd_implement_methodology <- function(row, outcomes, data, data_dictionary, all_outcome_params = NA, num_counties = NA){
  
  # Define variables from opts dataframe
  use_sdoh_scores <- as.numeric(row$use_sdoh_scores)
  use_sdoh_raw <- as.numeric(row$use_sdoh_raw)
  use_dems <- as.numeric(row$use_dems)
  remove_modifiable <- as.numeric(row$remove_modifiable)
  methodology <- as.character(row$methodology)
  meth_num <- as.numeric(row$meth_num)
  distance_list <- list()
  
  start_time <- Sys.time()
  print(methodology)
  print(start_time)
  
  # Loop through all outcomes
  n <- 1
  for(use_outcome in outcomes){
    print(paste("Outcome:", use_outcome))
    
    # if((methodology == "lasso euclidean dem" | methodology == "lasso euclidean all") & (use_outcome == "chronic_kidney_disease_pct")){
    #   debug(county_distance)
    # }
    
    orig_data <- data %>% 
      filter(!is.na(!!rlang::sym(use_outcome)))
    
    # Select variables to match on, limit data to these variables, and replace NAs
    use_data <- select_distance_columns(data = orig_data, data_dictionary = data_dictionary,
                                        sdoh_scores = use_sdoh_scores, sdoh_raw = use_sdoh_raw,
                                        outcome = use_outcome, dem = use_dems)
    
    n_rows <- nrow(all_outcome_params)
    model_params <- NA
    if(length(n_rows) != 0){
      out <- use_outcome
      model_params <- all_outcome_params %>% 
        filter(use_outcome == out)
    }
    
    # Get distance matrix using methodology specified
    dist_results <- county_distance(use_data, orig_data$fips, data_dictionary, methodology, use_outcome, remove_modifiable, model_params)
    distancem <- dist_results[1][[1]]
    mse <- dist_results[2][[1]]
    mtry <- dist_results[3][[1]]
    alpha <- dist_results[4][[1]]
    min_lambda <- dist_results[5][[1]]
    coefs_df <- dist_results[6][[1]]
    
    if(is.na(num_counties)){
      n_counties <- dim(distancem)[1]
    } else {
      n_counties <- num_counties
    }
    
    # Loop through counties
    for(county_num in c(1:n_counties)){
      data <- select_county(orig_data, distancem, county_num)
      
      ## Evaluate the methodology:
      # Look at the radar charts in order of county similarity
      # How similar are health outcomes of the top 5 most similar or similar within a certain distance?
      # They should have a median close to the county in question
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

# need to return the distance matrix
results <- upd_implement_methodology(opts, outcomes, data, data_dictionary)

# aggregate distance matrices for all outcomes
results[2]
# need to standardize all first because they are on different scales

# library(ape)
# for(i in c(1:8)){
#   # print(max(results[2][[1]][[i]]))
#   # print(dim(results[2][[1]][[i]]))
#   for(j in c(1:8)){
#     # test for similarity of two matrices
#     if(i != j){
#       print(paste0(outcomes[i]," vs. ", outcomes[j]))
#       print(mantel.test(as.matrix(results[2][[1]][[i]]), as.matrix(results[2][[1]][[j]]),
#                   graph = TRUE)$p)
#     }
#   }
# }

# null hypothesis - no relationship between the matrices
# Rejected - there is a relationship between the matrices
# some combos there is a relationship and some there isn't

# maybe just used pct kidney disease if this model looks good?
# or pct diabetic or pct obese?
# look at the coefficients
coefs_df <- results[3][[1]]
colnames(coefs_df)[2:9] <- outcomes
View(coefs_df)
colSums(!is.na(coefs_df))
# which matrices would be most general/incorporate the most data
# looking at the variables used, overobese_pct definitely uses the most
# followed by pct obese & the std spend vars, which we won't want to use
# let's just use the distance matrix for overobese_pct for now
# this also is a lower level outcome than the others (i.e. impacts them down the road, so makes sense to use)

final_distance <- as.data.frame(as.matrix(results[2][[1]][[8]]))
# TODO: county-level FIPS identifier for rows and columns
colnames(final_distance)
rownames(final_distance)
head(final_distance)
min_not_0 <- function(x){
  x <- x[x!=0]
  min(x)
}
apply(final_distance, 1, min_not_0)
# 8031 is denver - which is it most similar to?
final_distance %>% 
  dplyr::select('8031')
#8005 - part of denver county area

# the variables used in at least 4/8 models
coefs_df[which(rowSums(!is.na(coefs_df))>=5),"name"]

# output distance matrix with FIPS code as identifier
write.csv(final_distance, '../../data/final_distance.csv')
