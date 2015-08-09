#' Function to return the log-posterior for a parameter set
#' 
#' This function relies on existing lists in the environment (priors, Xmats) to 
#' calculate the log-posterior of those parameter coefficients.
#' 
#' @param v String containing the variable of interest
#' @param par Parameter vector of regression coefficients
#' 
logPostMultinom <- function(v, par) {
    
    #Prior contribution (assumes MVN)
    logPrior = dmvnorm(par,
                       mean = priors[[v]][["mean"]],
                       sigma = priors[[v]][["Sigma"]],
                       log = TRUE)
    
    #Likelihood contribution
    logLike = logLikMultinom(v, par)
    
    #Verify no NA's
    if (is.na(logPrior + logLike)) {
        print(v)
        print(outcome)
        print(logPrior)
        print(logLike)
        stop("Problem with LOGPOSTBIN")
    }
    
    #Return
    return(logPrior + logLike)
}
