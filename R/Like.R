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
#' @return Vector of likelihood contributions for v
#' 
Like = function(v, Xmat = NULL, par = NULL, log=FALSE){
    #Extract outcome
    outcome = get(v)
    while (is.character(outcome)) { outcome = get(outcome) }
    
    #Default to actual X matrix unless a special one is passed
    if (is.null(Xmat)) { Xmat = Xmats[[v]] }
    
    #Default to current value of parameter unless special one is passed
    if (is.null(par)) { par = params[[v]] }
    
    #Calculate eta, the linear predictor
    linpred = Xmat %*% par
    
    #Calculate likelihood or log-likelihood
    if (fam[[v]][["family"]] == "binomial"){
        if (log == FALSE){
            lik = ( fam[[v]][["linkinv"]](linpred) )^outcome + 
                (1 - fam[[v]][["linkinv"]](linpred) )^(1 - outcome)
        } else if (log == TRUE){
            lik = outcome*log( fam[[v]][["linkinv"]](linpred) ) +
                (1 - outcome)*log( 1 - fam[[v]][["linkinv"]](linpred) )
        }
    } else { stop("LIKE: family not supported") }
    
    #Return
    return(lik)
}


