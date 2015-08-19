#' Make a variance-covariance matrix essentially noninformative
#' 
#' Given a candidate prior variance-covariance matrix, this function
#' multiplies all, with the possible exception of the column/row corresponding
#' to the column/row passed in the "except" parameter.  The off-diagonal elements
#' of that row/column are multiplied by sqrt(nu).  This makes sense when you want 
#' (basically) non-informative priors for the vast majority of parameters but
#' need to have an informative prior on a single parameter.
#' 
#' @param covmat The original variance-covariance matrix
#' @param nu The inflation factor for the variances
#' @param except Character value containing the column for which you want to 
#' keep an informative prior (options).  This needs to match a column name of 
#' the variance-covariance matrix.
#' 
#' @return A rescaled, less-informative variance-covariance matrix
#' 
makeNoninformative <- function(covmat, nu = 10000, except = NULL){
    
    #Inflate all of the variances by a factor of nu
    covmat = as.matrix(covmat) * nu
    
    #If given the name of the column/row NOT to do this to, uninflate there
    if (!is.null(except)){
        #Extract which column
        whichCol = which(colnames(covmat) == except)
        
        #Uninflate by a factor of sqrt(nu) for the column
        covmat[,whichCol] = covmat[,whichCol] / sqrt(nu)
        
        #Uninflate by a factor of sqrt(nu) for the row
        covmat[whichCol,] = covmat[whichCol,] / sqrt(nu)
    }
    
    #Return
    return(covmat)
}

