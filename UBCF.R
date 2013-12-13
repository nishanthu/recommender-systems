# User-Based Collaborative Filtering
UBCF <- function(ui.matrix, ui.cm, ui.cm.num.na) {
  pred <- ui.matrix
  for(i in 1:nrow(ui.matrix)) {
    wt <- (1+ui.cm[i,]) / (1+ui.cm.num.na[i,])
    
    pred[i,] <- colMeans(ui.matrix/wt, na.rm=T)    
  }
  
  pred[is.na(pred)] <- 0
  ui.matrix[is.na(ui.matrix)] <- pred[is.na(ui.matrix)]
  
  # ToDo: Fix the final predicted values within range
  return(ui.matrix)
}
