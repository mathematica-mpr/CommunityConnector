setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)

dict <- read.csv('../../data/final_data_dictionary.csv')
dict %>% 
  filter(outcome == 1)

# 14 outcomes right now

# Percentage of adults aged 20 and above with diagnosed diabetes vs. Age-adjusted diagnosed diabetes percentage, 2016
# Age-adjusted obesity percentage, 2016 vs. Percentage of the adult population (age 20 and older) that reports a body mass index (BMI) greater than or equal to 30 kg/m2