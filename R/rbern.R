#' Sample Bernoulli random variables
#' 
#' Shortcut wrapper for rbinom()
#' 
#' @param prob Scalar or vector of probabilities
#' @param n The number of Bernoulli random variable, in case there is a common probability. 
#' Defaults to the length of prob
#' @return A vector of Bernoulli(prob) realizations
#' @examples
#' ##Generate variables with all different probabilities
#' rbern(seq(0.05,0.95,0.05)) 
#' 
rbern <- function(prob, n=length(prob), ...) { 
    rbinom(prob=prob, n=n, size=1, ...) 
}


