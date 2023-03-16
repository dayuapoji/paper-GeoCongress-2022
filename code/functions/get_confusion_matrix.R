get_confusion_matrix <- function(rf_results) {
  
  cm_df <- rf_results %>% .[(.$SoilLabel != 'Unlabeled'), ]
  cm_df$SoilLabel <- droplevels(cm_df$SoilLabel)
  cm <- confusionMatrix(cm_df$Prediction, cm_df$SoilLabel)
  
  return(cm)
}