#' Function to return the log-posterior for a parameter set
#' 
#' This function relies on existing lists in the environment (priors, Xmats) to 
#' calculate the log-posterior of those parameter coefficients.
#' 
#' @param v String containing the variable of interest
#' @param par Parameter vector of regression coefficients
#' 
logPost <- function(v, par) {
    
    if (isBinary(fam[[v]])) {
        #Binary
        logPostBin(v, par)
        
    } else if (isMultinomialLogit(fam[[v]])) {
        #Categorical
        logPostMultinom(v, par)
        
    } else { stop("Likelihood form not recognized by logPost function!") }
        
}


