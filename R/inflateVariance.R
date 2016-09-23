#' Multiply specified rows of a covariance matrix by an inflation factor
#' 
#' @param V Covariance matrix
#' @param inflation Multiplicative factor for inflation
#' @param indices Row/column indices for columns to inflate
#' 
#' @return Matrix the same shape as V 
#' @export
inflateVariance <- function(V, inflation = 1, indices = 1:ncol(V)){
  
  V[indices, ] <- sqrt(inflation) * V[indices, ]
  V[, indices] <- sqrt(inflation) * V[, indices]
  
  return(V)
  
}
