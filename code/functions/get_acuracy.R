get_accuracy <- function(cm) {
  
  accuracy <- cm$overall %>% t(.) %>% data.frame(.)
  
  return(accuracy)
}