rm(list = ls())
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

# need to return the distance matrix
results <- apply(opts, 1, implement_methodology, outcomes, data, data_dictionary)

# output distance matrix with FIPS code as identifier