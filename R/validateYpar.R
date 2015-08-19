#' ValidateY parameter vector (Y.par)
#' 
#' If Y has a log-link, it checks whether fitted values are all below 1.  If Y 
#' is a log-link (fam[["Y"]][["link"]] in the environment is log), then the check
#' will be performed. If the design matrix list Xmats exists and has element Y,
#' then it checks that Xmats[["Y"]] %*% par is less than 0 (meaning no fitted 
#' values out of range).  Otherwise, it checks to make sure that if all covariates
#' were in the range of (-1,1), no fitted values would be outside the [0,1] range
#' 
#' @param par Vector of candidate Y.par
#' @return FALSE if Y is log-link and has fitted values above 1; TRUE otherwise
#' @examples
#' ##Make environment variables
#' fam = list(Y = binomial(link="log"))
#' Zp = 1
#' Z = rbinom(n = 10, size = 1, prob = 0.5)
#' A = rbinom(n = 10, size = 1, prob = 0.5)
#' 
#' ##Candidate Y.pars (par1 should return TRUE, par2 should be FALSE)
#' par1 = c(-5, 1, 1, 2)
#' par2 = c(0.2, 0.1, -1)
#' 
#' ##First type of check, where Xmats[["Y"]] does not exist
#' validateYpar(par1)
#' validateYpar(par2)
#' 
#' #Second type of check, where Xmats[["Y"]] does exist
#' Xmats = list(Y = cbind(1,Z,A,U))
#' validateYpar(par1)
#' validateYpar(par2)
#' 
validateYpar <- function(par){
    
    #Only need to validate for log link
    if (fam[["Y"]][["link"]] != "log"){
        return(TRUE)
    } else {
        #Otherwise, check no fitted probabilities bigger than 1
        #This corresponds to eta (XBeta) values above 0
        #This check only works perfectly for binary variables right now!
        #May not be conservative enough if maximum values of variables
        #are above 1
        if (exists("Xmats")) {
            return( max(Xmats[["Y"]] %*% par) < 0 )    
        } else {
            return( sum(abs(par[-1])) < abs(par[1]) )
        }
    }
    
}
