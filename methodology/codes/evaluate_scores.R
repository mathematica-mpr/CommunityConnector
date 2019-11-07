setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)

data <- read.csv('../../data/final_data.csv')

cor(data[,names(data)[grepl("sdoh_score", names(data))]])
# Econ & Edu, Food, Community, & Healthcare
# Phys & Edu
# Educ & Food, Community, Healthcare
# Food & Community
# Community & Healthcare

# Different ways to adjust to bring correlations down/less driven by economic score
# 1. Divide by Econ Score, re-standardize
meth1 <- data.frame("sdoh_score_1" = data$sdoh_score_1, apply(data[,names(data)[grepl("sdoh_score", names(data)) & names(data) != "sdoh_score_1"]], 2,
                                                              function(x) x/data$sdoh_score_1))
cor(meth1)

# 2. Use correlation with Econ Score as a weight
# 3. Correlation squared?

# Standardize scores?? Standardize raw metrics score - mean/sd

# Measuring adjustments:
# scores should differentiate performance
# how well do the scores predict performance?
# relationship between one another