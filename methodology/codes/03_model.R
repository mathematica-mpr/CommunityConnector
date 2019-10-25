#########################################
############## Set up ###################
#########################################

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(dplyr)
library(randomForest)
library(caret)

data_dictionary <- read.csv('../../data/data_dictionary.csv')
data <- read.csv('../../data/data_sdoh_scores.csv')

#########################################
############# Clean data ################
#########################################

# remove sdoh vars
data <- data[,!names(data) %in% names(data)[grepl("sdoh_score", names(data))]]
data <- data[,!names(data) %in% c("Unnamed..0","fips","state","county","X")]

# TODO: move this to data processing
# remove the ratio variables permanently and use the rate instead
data <- data[,!names(data) %in% c("pcp.ratio","dentist.ratio","mhp.ratio","cohort.size",
                                  "other.pcp.ratio")]
# remove variables that I don't know what they are for now
data <- data[,!names(data) %in% c("labor.force","association.rate","presence.of.violation",
                                  "overcrowding","inadequate.facilities","age.adjusted.mortality")]

# change names of variables that are long
names(data)[names(data) == "median_value_.dollars._estimate_median_value_.dollars."] <- "median_value_doll"
names(data)[grepl("mean_usual_hours_worked",names(data))] <- "mean_hours_worked"

# TODO: test for all outcomes: % obese, % diabetic, age-adjusted mortality

# replace NAs with the mean since model won't work without
# data <- data.frame(lapply(data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
# na_count <-sapply(data, function(y) sum(length(which(is.na(y)))))
# na_count[na_count > 0]
# impute missing values in predictor data using proximity from randomForest
summary(data)
data <- rfImpute(X..obese ~., data)

glimpse(data)

#########################################
########### Model set up ################
#########################################

set.seed(1234)

# TODO: seems silly to use train/test?

trControl <- trainControl(method = 'cv',
                          number = 10,
                          search = 'grid')

#########################################
## Select number of variables mtry ######
#########################################

oob.err = double(13)
test.err = double(13)
# to get decorrelated trees, test number of variables to pull from
for(mtry in 1:80){
  rf <- randomForest(X..obese~.,
                     data = data,
                     method = 'rf',
                     metric = 'RMSE',
                     trControl = trControl, 
                     keep.forest = TRUE,
                     importance = TRUE,
                     mtry = mtry)
  oob.err[mtry] = rf$mse[350]
  pred = predict(rf)
  test.err[mtry] = with(data, mean( (X..obese-pred)^2 ))
}
matplot(1:mtry, cbind(test.err, oob.err), pch = 23, col = c("red", "blue"), type = "b", ylab="Mean Squared Error")
legend("topright", legend = c("OOB", "Test"), pch = 23, col = c("red", "blue"))
lines(predict(loess(test.err ~ c(1:mtry))), col = 'green')

get.elbow.points.indices <- function(x, y, threshold) {
  d1 <- diff(y) / diff(x) # first derivative
  d2 <- diff(d1) / diff(x[-1]) # second derivative
  indices <- which(abs(d2) > threshold)  
  return(indices)
}

get.elbow.points.indices(1:mtry, test.err, 0.6)
# mtry = 37 looks good!

# TODO:
# # Random forest cross-validation for feature selection
# # rfcv
# # and to tune alpha - to balance the depth of the tree & the goodness of fit
# # tuneRF for the optimal mtry parameter
# # MSE
# mean((data$X..obese - pred)^2)

rf <- randomForest(X..obese~.,
                   data = data,
                   method = 'rf',
                   metric = 'RMSE',
                   trControl = trControl, 
                   keep.forest = TRUE,
                   importance = TRUE,
                   proximity = TRUE,
                   mtry = 37)
rf
getTree(rf)
importance(rf)
imp <- varImp(rf, scale = FALSE)
print(imp)
varImpPlot(rf, n.var=20, sort = TRUE)
plot(rf)

# TODO: idea - which of the top 20 important variables are 'modifiable'
# leave these ones out. re-determine similiarity score based off differences in prediction
# look at different in SDoH radar and health outcomes

# proximity matrix using the random forest algorithm
# (same thing that's used for missing value imputation & outlier detection)
# could this give us a distance between counties?
# default proimity is calculated only using the trees where neither observation was included in the sample
# used to build that tree (they were out of bag)
# proximity is the proportion how often 2 data points end up in the same leaf node for different trees
rf$proximity

# OR use predictions in some way?
# make predictions
pred <- predict(rf)

# https://www.datacamp.com/community/tutorials/decision-trees-R
# boosting? useful when you have a lot of data and expect the decision trees to be very complex
library(gbm)
gbmod <- gbm(X..obese~., data = data, distribution = 'gaussian',
             shrinkage = 0.01, interaction.depth = 4)
gbmod.summ <- summary(gbmod, cBars = 25)
View(gbmod.summ)
# can look at relationship between prediction and x variables
plot(gbmod, i = "X..hispanic")

# other options for feature selection
library(FSelector)
ig <- information.gain(X..obese ~., data = data)
cutoff.k(ig, 2)

gain.ratio(X..obese ~., data = data)

sym <- symmetrical.uncertainty(X..obese ~ ., data = data)
cutoff.biggest.diff(sym)