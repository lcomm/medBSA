#' Generate Z variables (confounders) for simulation
#' 
#' This function generates a dataset of size ss with the Z parameters located
#' in the params[[Z]] list.  The Z's are created successively, so the Z1 depends 
#' on  nothing, while Z2 can depend on Z1, and so on.  The dimension of Z is 
#' determined by Zp.  When Zp > 1, params[["Z"]] should be list of lists containing
#' parameter vectors.
#' 
#' @return A vector (if Zp = 1) or matrix (Zp > 1) of Z variables
#' @seealso genVar
#' 
#' @examples
#' ##Variables that need to exist in environment
#' ss = 10
#' Zp = 2
#' fam = list(Z = list(binomial(link="logit"), binomial(link="logit")))
#' params = list(Z = list( c(0), c(0,1) ) )
#' ##Make a Z matrix
#' makeZ()
#' 
makeZ <- function(){
    #Start design matrix for Z
    Xmat = matrix(1, nrow=ss, ncol=1)
    #Make
    for (i in 1:Zp){
        Zt = genVar(fam$Z[[i]], Xmat, params$Z[[i]])
        Xmat = cbind(Xmat, Zt)
    }
    #Return a full matrix of Z at the end
    Z = Xmat[,-1]
    if (Zp == 1) { Z = as.matrix(Z, ncol=1, nrow=ss) }
    if (exists("Znames")){
        colnames(Z) = Znames
    } else {
        colnames(Z) = paste0("Z",1:Zp)
    }
    
    return(Z)
}

