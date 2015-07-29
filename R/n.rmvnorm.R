#' An as.numeric() version of a mvtnorm::rmvnorm()
#' 
#' Useful for ensuring sampled vectors will conform in matrix multiplication
#' Passes all arguments directly to rmvnorm()
#' 
#' @examples
#' ##Draw a multivariate random vector
#' n.rmvnorm(n = 1, mean = 1:3)
#' 
n.rmvnorm <- function(...){ 
    as.numeric(rmvnorm(...)) 
}


