#' Obtain log-likelihood of binary variable
#' 
#' This function returns the log-likelihood of binary data.  Relies on the 
#' existence of Xmats[[v]] and fam[[v]] containing the design matrix and link 
#' functions for v.
#' 
#' @param v String containing variable name
#' @param par Parameter vector of regression coefficients 
#' 
logLikBin = function(v, par){
    
    #Get outcome variable
    outcome = get("v")
    
    #Evaluate another time if still character (means we are more nested)
    while (is.character(outcome)) { outcome = get(outcome) }
    
    #Calculate log-likelihood from all n subjects
    if (fam[[v]][["link"]] %in% c("logit", "log")) {
        
        #Calculate p(v = 1 | X, par) based on the link function and the coefficients' value
        p = fam[[v]][["linkinv"]](Xmats[[v]] %*% par)
        
        #Log-likelihood of bernoulli
        ll = sum(dbinom(x = outcome, prob = p, size = 1, log = TRUE))
    
        #Return
        return(ll)
        
    } else { stop("LOGLIKBIN: Link not supported!") }
}

