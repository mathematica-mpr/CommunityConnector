rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
install.packages("randomForest")
library(randomForest)
library(caret)

data_dictionary <- read.csv('../../data/data_dictionary.csv')
data <- read.csv('../../data/data_sdoh_scores.csv')
# remove variables that have all NAs
data <- data[,colSums(is.na(data))<nrow(data)]
# replace NAs with the mean since model won't work without
# TODO: impute missing values in predictor data using proximity from randomForest
# rfImpute()
data <- data.frame(lapply(data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count[na_count > 0]

glimpse(data)

trControl <- trainControl(method = 'cv',
                          number = 10,
                          search = 'grid')

set.seed(1234)

# TODO: lots more parameters
rf <- randomForest(X..Obese~.,
                   data = data,
                   method = 'rf',
                   metric = 'RMSE',
                   trControl = trControl, 
                   keep.forest = TRUE,
                   importance = TRUE)
rf
getTree(rf)
importance(rf)
varImp(rf)
varImpPlot(rf, n.var=10)
varUsed(rf, by.tree = FALSE, count = TRUE)
varUsed(rf, by.tree = TRUE, count = TRUE)
varUsed(rf, by.tree = FALSE, count = FALSE)

# Random forest cross-validation for feature selection
# rfcv

# tuneRF for the optimal mtry parameter
