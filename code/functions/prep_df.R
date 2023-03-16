prep_df <- function(rf_chainage) {
  
  output <- rf_chainage %>% 
    # remove unused columns
    select(!contains('GeologicSegment')) %>%
    select(!contains('Chainage')) %>%
    select(!contains('Ring')) %>%
    # set label as factor
    mutate(., SoilLabel = factor(as.character(.$SoilLabel)))
  
  return(output)
}