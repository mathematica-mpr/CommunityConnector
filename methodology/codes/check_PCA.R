setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)

dict <- read.csv('../../data/data_dictionary.csv')
spca <- read.csv('../../data/DictionaryPostSPCA.csv')

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

# Get rid of Food Environment Index, pct mental distress, excessive drinking,
# short hospital rates, medicare adjusted pp cost and Add physically inactive
# no kidney-related vars in scores
# all budget variables included
# pct long commute & pct drive alone in 2 potential inputs
# no mean hours or % same tract
# physically inactive added

# from 1:
# removed medicare_std_adj_cost_pp

# from 2:
# removed median_value (WHY??), pct_with_access, pct_good_air
# added: comm_hlth_cntrs_pp_rate
# TODO: change pct_phs_inactive to using walkability instead
# added: walk_score - why? maybe not up to date with the html?
# need to add: budget_water

# 3: removed pct
# 4:
# 5:
# 6:
# 7:

# TODO: still need the PC variation and the loading for the variables we add


