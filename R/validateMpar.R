#' Validate M parameter vector (M.par)
#' 
#' If M has a log-link, it checks whether fitted values are all below 1.  If M 
#' is a log-link (fam[["M"]][["link"]] in the environment is log), then the check
#' will be performed. If the design matrix list Xmats exists and has element M,
#' then it checks that Xmats[["M"]] %*% par is less than 0 (meaning no fitted 
#' values out of range).  Otherwise, it checks to make sure that if all covariates
#' were in the range of (-1,1), no fitted values would be outside the [0,1] range
#' 
#' @param par Vector of candidate M.par
#' @return FALSE if M is log-link and has fitted values above 1; TRUE otherwise
#' @examples
#' ##Make environment variables
#' fam = list(M = binomial(link="log"))
#' Zp = 1
#' Z = rbinom(n = 10, size = 1, prob = 0.5)
#' A = rbinom(n = 10, size = 1, prob = 0.5)
#' 
#' ##Candidate M.pars (par1 should return TRUE, par2 should be FALSE)
#' par1 = c(0.2, 0.1, 2)
#' par2 = c(0.2, 0.1, -2)
#' 
#' ##First type of check, where Xmats[["M"]] does not exist
#' validateMpar(par1)
#' validateMpar(par2)
#' 
#' #Second type of check, where Xmats[["M"]] does exist
#' Xmats = list(M = cbind(1,Z,A))
#' validateMpar(par1)
#' validateMpar(par2)
#' 
validateMpar <- function(par){
    
    #Only need to validate for log link
    if (fam[["M"]][["link"]] != "log"){
        return(TRUE)
    } else {
        #Otherwise, check no fitted probabilities bigger than 1
        #This corresponds to eta (XBeta) values above 0
        #This check only works perfectly for binary variables right now!
        if (exists("Xmats$M")) {
            return( max(Xmats[["M"]] %*% par) < 0 )    
        } else {
            return( sum(abs(par[-1])) < abs(par[1]) )
        }
    }
    
}

