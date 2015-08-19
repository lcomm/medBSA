#' Calculate the likelihood of a variable given its design matrix and parameter
#' 
#' This function calculates the likelihood or log-likelihood of a variable given a
#' vector of realizations.  If Xmat and/or par are not explicitly provided, the 
#' function extracts these from lists in the environment (Xmats and params)
#' 
#' @param v String containing the variable name
#' @param Xmat Optional parameter to specify design matrix used in linear predictor
#' part of v
#' @param par Optional parameter to specify parameter vector used in linear predictor
#' part of v
#' @param log Should the log-density be returned? Defaults to FALSE
#' @param summation Should the log-likelihoods be summed across the subjects (or 
#' likelihoods multiplied out)?  Defaults to TRUE
#' @return Vector of likelihood contributions for v
#' 
Like = function(v, Xmat = NULL, par = NULL, log = FALSE, summation = TRUE){
    
    #Extract outcome
    outcome = get(v)
    while (is.character(outcome)) { outcome = get(outcome) }
    
    #Default to actual X matrix unless a special one is passed
    if (is.null(Xmat)) { Xmat = Xmats[[v]] }
    
    #Default to current value of parameter unless special one is passed
    if (is.null(par)) { par = params[[v]] }
    
    #Calculate log-likelihood based on data type of v
    if (isBinary(fam[[v]])){
        ll = logLikBin(v, par, Xmat, summation)
    } else if (isMultinomialLogit(fam[[v]])){
        ll = logLikMultinom(v, par, Xmat, summation)
    } else { stop("Only binary and categorical data types supported at this time") }
        
    #Return likelihood in requested form
    if (log == TRUE){
        return(ll)
    } else {
        return(exp(ll))
    }
    
}


