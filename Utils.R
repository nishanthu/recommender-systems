# Generate User-Item rating matrix from User-Item-Rating data frame
# Missing ratings will be replaced by NAs
# User ID and Item ID should be from 1..N
User.Item.Matrix <- function(ui.df) {
  stopifnot(identical(class(ui.df), "data.frame"), nrow(ui.df)==3)
  
  ui.matrix <- matrix(NA, length(unique(ui.df[,1])), length(unique(ui.df[,2])))
  for (i in 1:nrow(ui.df)) {
    ui.matrix[ui.df[i,1], ui.df[i,2]] <- ui.df[i,3] 
  }
  
  return(ui.matrix)
}

# Generate User-Item correlation matrix
User.Item.Corr <- function(ui.matrix) {
  stopifnot(identical(class(ui.matrix), "matrix"))
  
  require(cluster)
  ui.cm <- as.matrix(daisy(m,metric="manhattan"))  
  diag(ui.cm) <- Inf
  cm[is.na(ui.cm)] <- Inf
  
  return(ui.cm)
}

# Generate Item-Item correlation matrix
Item.Item.Corr <- function(ui.matrix) {
  stopifnot(identical(class(ui.matrix), "matrix"))
  
  require(cluster)
  ii.cm <- as.matrix(daisy(t(m),metric="manhattan"))
  diag(ii.cm) <- Inf
  cm[is.na(ii.cm)] <- Inf
  
  return(ii.cm)
}

# Get the matrix of (combined) NA values which were skipped 
# during the computation of User-Item correlation matrix
User.Item.Corr.Num.NAs <- function(ui.matrix) {
  ui.cm.num.na <- ifelse(is.na(ui.matrix), 0 ,1)
  ui.cm.num.na <- ui.cm.num.na %*% t(ui.cm.num.na)
  ui.cm.num.na <- ncol(ui.matrix) - ui.cm.num.na
  
  return(ui.cm.num.na)
}
