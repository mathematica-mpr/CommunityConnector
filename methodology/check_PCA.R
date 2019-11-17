setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)

dict <- read.csv('../../data/data_dictionary.csv')
spca <- read.csv('../../data/DictionaryPostSPCA.csv')

# review potential inputs vs. ones actually selected
# make sure aligns with .html
for(i in c(1:6)){
  merged <- dict %>% 
    filter(!!rlang::sym(paste0("used_sdoh_",i)) == 1) %>% 
    select(column_name) %>% 
    mutate(potential = 1) %>% 
    merge(spca %>% 
            filter(sdoh_Category == i), by.x = "column_name",
          by.y = "Variable_Name", all.x = TRUE, all.y = TRUE) %>% 
    select(-Variance_Explained, - Loading)
  print(merged)
}

# Notes from meetings that should be incorporated:
# Get rid of Food Environment Index, pct mental distress, excessive drinking,
# short hospital rates, medicare adjusted pp cost and Add physically inactive
# no kidney-related vars in scores
# all budget variables included
# pct long commute & pct drive alone in 2 potential inputs
# no mean hours or % same tract
# physically inactive added

# from 1: removed medicare_std_adj_cost_pp
# from 2: added budget_water
# 3: 
# 4: removed food_environment_index
# 5: remove pct_frequent_mental_distress
# 6: removed short_hosp_pp_rate

# make sure we have all of the PC variation and loadings for variables that are selected
spca %>% 
  filter(!is.na(sdoh_Category))