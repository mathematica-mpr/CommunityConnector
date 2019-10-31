# setup
use_outcome <- outcomes[1]
orig_data <- data %>% 
  filter(!is.na(!!rlang::sym(use_outcome)))
use_data <- select_distance_columns(data = orig_data, data_dictionary = data_dictionary,
                                    sdoh_scores = 0, sdoh_raw = 1,
                                    outcome = use_outcome, dem = 1)
use_data <- replace_nas_rf(use_data, use_outcome)
formula <- as.formula(paste(use_outcome,"~."))
metric <- "RMSE"
seed <- 1234

# Cross Validation
# reference: https://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/
###################################
# random search

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(use_data))
rf_random <- train(formula, data=use_data, method="rf", metric=metric, tuneLength=15, trControl=control, proximity = TRUE)
rf_random
print(rf_random)
plot(rf_random)
summary(rf_random)
class(rf_random)
predict(rf_random)

# most accurate: 64

###################################
# grid search

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
set.seed(seed)
tunegrid <- expand.grid(.mtry=c(1:ncol(use_data)))
rf_gridsearch <- train(formula, data=use_data, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)
rf_gridsearch$bestTune
# best = 58

###################################
# algorithm tools
set.seed(seed)
bestmtry <- tuneRF(use_data[,!names(use_data) %in% use_outcome],
                   use_data[,names(use_data) %in% use_outcome],
                   ntree = 500, stepFactor = 0.1)
print(bestmtry)
# i don't get this one

###################################

# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(use_data))))
modellist <- list()
for (ntree in c(1000, 1500, 2000, 2500)) {
  set.seed(seed)
  fit <- train(formula, data=use_data, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control, ntree=ntree)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

# past 100 trees, doesn't really make a difference
# default is 500 so we're good

###################################
# extend caret
customRF <- list(type = "Regression", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
control <- trainControl(method="repeatedcv", number=10, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(1000, 1500, 2000, 2500))
set.seed(seed)
custom <- train(formula, data=use_data, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)
# there isn't much interactivity between ntree and mtry and it takes a really long time, so don't worry about tuninng ntree