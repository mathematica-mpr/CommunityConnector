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

# Cross Validation

###################################
# random search

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(1234)
mtry <- sqrt(ncol(use_data))
rf_random <- train(formula, data=use_data, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

# most accurate: 

###################################

# Manual Search
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="grid")
control
tunegrid <- expand.grid(.mtry=c(sqrt(ncol(use_data))))
modellist <- list()
for (mtry in c(1:ncol(use_data)) {
  set.seed(1234)
  fit <- train(formula, data=use_data, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control)
  key <- toString(ntree)
  modellist[[key]] <- fit
}
# compare results
results <- resamples(modellist)
summary(results)
dotplot(results)

# past 100 trees, doesn't really make a difference
# default is 500 so we're good