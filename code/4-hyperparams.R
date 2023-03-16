# ==============================================================================
# HYPERPARAMETER TUNING
# ==============================================================================

set.seed(1)

rf_df_labeled <- rf_df %>%
  .[(.$SoilLabel != 'Unlabeled'), ]

# training data set
train_index <- sample(nrow(rf_df_labeled), 0.8 * nrow(rf_df_labeled), 
                      replace = FALSE)
rf_train_chainage <- rf_df_labeled[train_index, ]
rf_train <- prep_df(rf_train_chainage)

# up-sampling for imbalance data
rf_train_up <- upSample(x = rf_train[1:(ncol(rf_train)-1)],
                        y = factor(rf_train$SoilLabel),
                        yname = "SoilLabel") 

# set train control
p <- ncol(rf_train)-1 # number of predictors

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     verboseIter = FALSE,
                     search = 'grid')

tunegrid <- expand.grid(mtry = c(1, 5, round(sqrt(p)), round(p/3), 15, round(p/2), 20, 25, 30, p),
                        splitrule = c("gini", "extratrees"),
                        min.node.size = c(1, 3, 5, 10, 20))

# initialize list
cv_results_list <- NULL

for (numtrees in c(1, 5, 10, 50, seq(100, 1000, 100))) {
  
  # training
  rf_model <- train(SoilLabel~., data = rf_train_up,
                    method = 'ranger',
                    trControl = ctrl,
                    tuneGrid = tunegrid,
                    num.trees = numtrees,
                    num.threads = 7,
                    verbose = FALSE)
  
  # save results in a data frame
  cv_results <- data.frame(rf_model$results, 
                           ntrees = rep(numtrees, each = nrow(rf_model$results)))
  
  # append
  cv_results_list <- rbind(cv_results_list, cv_results)
  
  # computation progress
  print(paste("numtrees = ", numtrees, "done"))
}

# save as csv
write.csv(cv_results_list,'../results/cv_results.csv', row.names = FALSE)

