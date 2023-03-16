# ==============================================================================
# RANDOM FORESTS STATIC CLASSIFICATION 80:20
# ==============================================================================

set.seed(1)

# ------------------------------------------------------------------------------
# DATA SPLITTING
# ------------------------------------------------------------------------------

rf_df_labeled <- rf_df %>% .[(.$SoilLabel != 'Unlabeled'), ]

# training data set
train_index <- sample(nrow(rf_df_labeled), 0.8 * nrow(rf_df_labeled), 
                      replace = FALSE)
rf_train_chainage <- rf_df_labeled[train_index, ]
rf_train <- prep_df(rf_train_chainage)

# up-sampling for imbalance data
rf_train_up <- upSample(x = rf_train[1:(ncol(rf_train)-1)],
                        y = factor(rf_train$SoilLabel),
                        yname = "SoilLabel") 

# testing data set
rf_test_chainage <- rf_df %>% filter(Chainage %!in% rf_train_chainage$Chainage)
rf_test <- prep_df(rf_test_chainage)


# ------------------------------------------------------------------------------
# TRAINING MODEL
# ------------------------------------------------------------------------------

# number of predictors
p <- ncol(rf_train)-1 

# train model
rf_prob_model <- ranger(SoilLabel~., data = rf_train_up, 
                        probability = TRUE,
                        num.trees = 500,
                        mtry = round(sqrt(p)),
                        min.node.size = 10,
                        splitrule = "extratrees")

# ------------------------------------------------------------------------------
# TESTING & PREDICTIONS
# ------------------------------------------------------------------------------

# predict
rf_predictions <- predict(rf_prob_model, data = rf_test)$predictions

# max probability
max_prob <- NULL
for (i in 1:nrow(rf_predictions)) {
  col_index <- which(rf_predictions[i, ] == max(rf_predictions[i, ]))
  pred <- colnames(rf_predictions)[col_index]
  max_prob <- rbind(max_prob, pred)
}

# create results df
rf_results <- data.frame(
  Ring = rf_test_chainage$Ring,
  Chainage = rf_test_chainage$Chainage,
  SoilLabel = rf_test$SoilLabel, 
  rf_predictions,
  Prediction = factor(max_prob),
  Error = ifelse((as.character(rf_test$SoilLabel) == 'Unlabeled'), 0, 
                 ifelse((as.character(rf_test$SoilLabel) == max_prob), 0, 1)))

# ------------------------------------------------------------------------------
# MODEL EVALUATION
# ------------------------------------------------------------------------------

# measure classification accuracy
num_error <- rf_results[which(rf_results[, 'Error'] == 1), ]
num_label <- rf_results[which(rf_results[, 'SoilLabel'] != 'Unlabeled'), ]

accuracy_static <- ((nrow(num_label) - nrow(num_error))/nrow(num_label))

