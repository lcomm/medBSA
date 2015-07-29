#' Validate U parameter vector (U.par)
#' 
#' Checks to see whether gamma_1 is positive
#' Tries to detect which parameter is gamma_1 based on the labels of the design 
#' matrix (relies on Xmats[["U"]] existing in the environment), but if that fails 
#' it assumes it is the first regression coefficient after the Z variables (relies on
#' Zp being in the environment)
#' 
#' @param par Vector of candidate U.par
#' @return TRUE if the gamma_1 element of par is positive; FALSE otherwise
#' @examples
#' ##Make environment variables
#' Zp = 1
#' Z = rbinom(n = 10, size = 1, prob = 0.5)
#' A = rbinom(n = 10, size = 1, prob = 0.5)
#' 
#' ##Candidate U.pars (par1 should return TRUE, par2 should be FALSE)
#' par1 = c(0.2, 0.1, 2)
#' par2 = c(0.2, 0.1, -2)
#' 
#' ##First type of check, where Xmats[["U"]] does not exist
#' validateUpar(par1)
#' validateUpar(par2)
#' 
#' #Second type of check, where Xmats[["U"]] does exist
#' Xmats = list(U = cbind(1,Z,A))
#' validateUpar(par1)
#' validateUpar(par2)
#' 
validateUpar <- function(par){
    
    #Figure out which column of the U design matrix corresponds to A
    #Defaults to no-mediator case
    if (exists("Xmats$U")) {
        whichGam1 = which(colnames(Xmats[["U"]]) == "A")
    } else {
        whichGam1 = Zp + 2
    }
    
    #Return whether gamma1 element if non-negative or not
    return(par[whichGam1] >= 0)
}
