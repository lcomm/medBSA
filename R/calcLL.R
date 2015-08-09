#' Log-likelihood
#' 
#' Function to evaluate the total log-likelihood at a certain parameter set
#' 
#' @param v String containing variable name
#' @param par Parameter vector
#' 
#' @return The sum of the log-likelihood of the data, as evaluated at "par"
#' 
calcLL <- function(v, par){
    
    #Direct to appropriate log-likelihood function based on family
    if (isMultinomialLogit(fam[[v]])){
        
        #Multinomial log-likelihood
        logLikMultinom(v, par)
        
    } else if (isBinary(fam[[v]])){
        
        #Binomial  (binary, actually) log-likelihood
        logLikBin(v, par)
        
    } else {
        
        #Unsupported
        stop("Log-likelihood calculation failed. Link/variable type not supported.")
        
    }
}

