#' Density of Bernoulli random variables
#'
#' This wrapper for dbinom() returns the density of one or more Bernoulli random
#' variables. Obtain the log-densities by passing "log=TRUE".
#'
#' @param x Observed values of the Bernoulli R.V.
#' @param prob Scalar or vector of probabilities
#' @return A vector containing the densities (or log-densities) of the observed values
#' @examples
#' ##Get density of realizations with all different probabilities
#' dbern(x = c(1,0,0,1), prob = c(0.5,0.2,0.3,0.4))
#'
#' @export
dbern <- function(x, prob, ...){
    dbinom(x=x, size=1, prob=prob, ...)
}
