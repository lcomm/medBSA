#' Calculate log-likelihood of multinomial random variable 
#' 
#' This function returns the log-likelihood of multinomial data.  Relies on the 
#' existence of Xmats[[v]] and fam[[v]] containing the design matrix and link 
#' functions for v.
#' 
#' @param v String containing variable name
#' @param par.v Parameter vector of regression coefficients 
#' @param Xmat Optional design matrix for computing log-likelihood; if none is
#' specified, Xmats[[v]] is used
#' @param summation Whether the individual likelihood contributions should be summed;
#' defaults to TRUE
#' 
logLikMultinom <- function(v, par.v, Xmat = NULL, summation = TRUE){
    
    #Get outcome variable
    outcome = get("v")
    
    #Evaluate another time if still character (means we are more nested)
    while (is.character(outcome)) { outcome = get(outcome) }
    
    #If the variable is just a vector, we need to expand to get the dummy matrix
    if (is.vector(outcome)) {
        outcome = makeMultinomialDummy(outcome, dropRef = FALSE)
    }
    
    #If the rows do not all sum to 1, add the 1st column in order as indicator
    #of the reference level
    if (sum(outcome) != nrow(outcome)) {
        outcome <- cbind(1-rowSums(outcome), outcome)
    }

    #Extract design matrix
    if (is.null(Xmat)) { Xmat = Xmats[[v]] }
    
    #Figure out if par.v is a vector or a matrix
    #If a vector, need to reshape into a form suitable for applying
    #inverse multilogit function
    if (is.vector(par.v)){
        
        #Number of parameters in each logit model
        nParam = ncol(Xmat)
        
        #Reshape to parameter matrix
        par.v = matrix(par.v, nrow = nParam, byrow = TRUE)
        
    }

    #Get probabilities for each category
    probs = multilogit(Xmat %*% par.v, inverse=TRUE)
    
    #Get log-likelihood or summed log-likelihood, depending
    if (summation == TRUE){
        #Calculate log-likelihood from all n subjects
        #Extract only the realized probabilities, then sum the logs of those 
        ll = sum(log(probs[as.logical(outcome)]))
    } else {
        #Calculate log-likelihood for each subject individually
        ll = log(probs[as.logical(outcome)])
    }
    
    #Return
    return(ll)
    
}

