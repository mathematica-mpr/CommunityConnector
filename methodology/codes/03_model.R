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
data <- data.frame(lapply(data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
na_count[na_count > 0]

glimpse(data)

trControl <- trainControl(method = 'cv',
                          number = 10,
                          search = 'grid')

set.seed(1234)

rf_default <- train(X..Obese~.,
                    data = data,
                    method = 'rf',
                    metric = 'Accuracy',
                    trControl = trControl)
table(is.na(data$X..Obese), exclude = NULL)
