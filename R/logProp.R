#' Calculate log-density of (multivariate normal) proposal distribution
#' 
#' Based on the props list existing in the environment, this function calculates
#' the log-density of the proposal distribution evaluated at par
#' 
#' @param v String containing variable name
#' @param par Value of parameter vector at which to evaluate log-density
#' 
logProp = function(v, par) {
    dmvnorm(par, 
            mean = props[[v]][["mean"]],
            sigma = props[[v]][["Sigma"]],
            log = TRUE)
}

