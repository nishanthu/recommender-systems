# Low Rank Matrix Factorzation using SVD
LRMF <- function(ui.matrix) {
  means <- rowMeans(ui.matrix, na.rm=T)
  ui.matrix <- ui.matrix - means
  m0 <- ui.matrix
  m0[is.na(ui.matrix)] <- 0
  svd.m <- svd(m0)
  
  K <- floor(0.1*nrow(ui.matrix))
  m.final <- matrix(NA, nrow(ui.matrix), ncol(ui.matrix))
  for(i in 1:nrow(ui.matrix)) {
    for (j in 1:ncol(ui.matrix)) {
      m.final[i,j] <- (((svd.m$u[i,1:K]*sqrt(svd.m$d[1:K]))%*%(sqrt(svd.m$d[1:K])*t(svd.m$v)[1:K,j]))[1,1])
    }
  }
  m.final[!is.na(ui.matrix)] <- m[!is.na(ui.matrix)]
  
  return(m.final)
}
