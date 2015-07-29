#' Draw a parameter set from the (MVN) prior distribution
#' 
#' This draws from a multivariate normal density
#' It relies on the fact that there is an existing list in the environment called 
#' "priors" that has a sublist with the variable's name and that sublist contains 
#' the mean and variance-covariance matrix of the parameter's prior distribution
#' 
#' @param v The name of the variable, as a string
#' @return A parameter vector drawn from the prior distribution
#' @seealso n.rmvnorm 
#' @examples
#' ##Make the prior list
#' priors = list(Y = list(mean = c(0,0), sigma = diag(c(1,1))))
#' 
#' ##Draw a vector from the prior
#' drawParamFromPrior(v = "Y")
#' 
drawParamFromPrior <- function(v){
    return(n.rmvnorm(1, mean = priors[[v]][["mean"]], sigma = priors[[v]][["Sigma"]]))
}

