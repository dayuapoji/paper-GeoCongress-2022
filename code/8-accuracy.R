# ==============================================================================
# SINGLE FEATURE ACCURACY
# ==============================================================================

set.seed(1)

# extract feature names
features <- names(rf_df)[1:(ncol(rf_df)-4)] 

# initialize list
accuracy_feature <- NULL

# get accuracy for single feature
for (feature in features) {

  rf_feat <- rf_df %>%
    .[c(feature,
        'Ring', 'Chainage', 'SoilLabel', 'GeologicSegment')]
  
  # get RF predictions
  rf_results <- get_static_predictions(rf_feat, train_frac = 0.8)
  # get confusion matrix
  cm <- get_confusion_matrix(rf_results)
  # get accuracy
  accuracy <- get_accuracy(cm)
  accuracy$Feature <- feature

  accuracy_feature <- rbind(accuracy_feature, accuracy)
}

# set factor level
accuracy_feature$Feature <- factor(accuracy_feature$Feature,
                                   levels = accuracy_feature$Feature)
