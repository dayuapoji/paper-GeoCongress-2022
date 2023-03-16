# ==============================================================================
# RANDOM FORESTS SEQUENTIAL DYNAMIC CLASSIFICATION
# ==============================================================================

set.seed(1)

# ------------------------------------------------------------------------------
# DYNAMIC MODEL
# ------------------------------------------------------------------------------

# initial
rf_results_list <- NULL

# loop for dynamic prediction
for (i in  2:(nrow(rf_df)-1)) {
  
  # training data set
  rf_train <- rf_df[1:i, ] %>% 
    select(!contains('GeologicSegment')) %>%
    select(!contains('Chainage')) %>%
    select(!contains('Ring')) %>%
    .[(.$SoilLabel != 'Unlabeled'), ] %>%
    mutate(., SoilLabel = factor(as.character(.$SoilLabel)))
  
  # upsampling for imbalance data
  rf_train_up <- upSample(x = rf_train[, !(colnames(rf_train) %in% c('SoilLabel'))],
                          y = factor(rf_train$SoilLabel)) %>%
    rename(SoilLabel = Class)
  
  # testing data set
  rf_test <- rf_df[(i+1), ] %>%
    select(!contains('GeologicSegment')) %>%
    select(!contains('Chainage')) %>%
    select(!contains('Ring')) %>%
    mutate(., SoilLabel = factor(as.character(.$SoilLabel)))
  
  # train model
  rf_prob_model <- ranger(SoilLabel~., data = rf_train_up, 
                          probability = TRUE,
                          num.trees = 500,
                          mtry = round(sqrt(p)),
                          min.node.size = 10,
                          splitrule = "extratrees")

  # predict
  rf_predictions <- data.frame(predict(rf_prob_model, data = rf_test)$predictions)
  
  # max probability
  col_index <- which(rf_predictions == max(rf_predictions))
  max_prob <- colnames(rf_predictions)[col_index]
  
  # create results df
  rf_results <- data.frame(
    Ring = rf_df$Ring[i+1],
    Chainage = rf_df$Chainage[i+1],
    SoilLabel = rf_test$SoilLabel, 
    CCS = ifelse(is.null(rf_predictions[['CCS']]), 0, rf_predictions[['CCS']]),
    CCS_CSG = ifelse(is.null(rf_predictions[['CCS_CSG']]), 0, rf_predictions[['CCS_CSG']]),
    CSGCSF_CCS = ifelse(is.null(rf_predictions[['CSGCSF_CCS']]), 0, rf_predictions[['CSGCSF_CCS']]),
    CSGCSF_TLTD = ifelse(is.null(rf_predictions[['CSGCSF_TLTD']]), 0, rf_predictions[['CSGCSF_TLTD']]),
    TLTD_CSGCSF_CCS = ifelse(is.null(rf_predictions[['TLTD_CSGCSF_CCS']]), 0, rf_predictions[['TLTD_CSGCSF_CCS']]),
    Prediction = factor(max_prob),
    Error = ifelse((as.character(rf_test$SoilLabel) == 'Unlabeled'), 0, 
                   ifelse((as.character(rf_test$SoilLabel) == max_prob), 0, 1)))
  
  # bind all results
  rf_results_list <- rbind(rf_results_list, rf_results)
  
  # computation progress
  print(paste('done i =', i, 'progress =', round((i/(nrow(rf_df)-1))*100, 2), '%'))
}

# ------------------------------------------------------------------------------
# MODEL EVALUATION
# ------------------------------------------------------------------------------

# measure classification accuracy
num_error <- rf_results_list[which(rf_results_list[, 'Error'] == 1), ]
num_label <- rf_results_list[which(rf_results_list[, 'SoilLabel'] != 'Unlabeled'), ]

accuracy_dynamic <- ((nrow(num_label) - nrow(num_error))/nrow(num_label))

