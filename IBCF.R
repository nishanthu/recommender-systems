# Item-Based Collaborative Filtering
IBCF <- function(ui.matrix, ii.cm) {
  pred <- ui.matrix
  for(i in 1:nrow(ui.matrix)) {
    for(j in 1:ncol(ui.matrix)) {
      pred[i,j] <- mean(ui.matrix[i,] / (1+ii.cm[j,]), na.rm=T)    
    }
  }
  
  pred[is.na(pred)] <- 0
  ui.matrix[is.na(ui.matrix)] <- pred[is.na(ui.matrix)]
  
  # ToDo: Fix the final predicted values within range
  return(ui.matrix)
}
