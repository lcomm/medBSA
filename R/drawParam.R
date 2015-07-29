#' Draw a parameter set from the (MVN) proposal distribution
#' 
#' This draws from a multivariate normal density
#' It relies on the fact that there is an existing list in the environment called 
#' "props" that has a sublist with the variable's name and that sublist contains 
#' the mean and variance-covariance matrix of the parameter's proposal distribution
#' 
#' @param v The name of the variable, as a string
#' @return A parameter vector drawn from the proposal distribution
#' @seealso n.rmvnorm 
#' @examples
#' ##Make the props list
#' props = list(Y = list(mean = c(0,0), sigma = diag(c(1,1))))
#' 
#' ##Draw a vector from the multivariate normal proposal distribution
#' drawParam(v = "Y")
#' 
drawParam <- function(v){
    return(n.rmvnorm(1, props[[v]][["mean"]], props[[v]][["Sigma"]]))
}


