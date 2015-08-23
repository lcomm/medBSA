#' Calculate average residual disparity
#' 
#' This function calculates the average residual disparity.  If the treatment A
#' can be a cause, this is equivalent to the controlled direct effect, 
#' marginalized over the distribution of the confounders as observed in the A = 1
#' population.  For this to be meaningful as a residual disparity measure, A = 1
#' should be the relatively disadvantaged population (i.e., the one we would
#' intervene on).  Function requires A, Y.par, U.par, M.par, fam, and Xmats to 
#' be in the environment
#' 
#' @return ARD The average residual disparity after intervening on M
#' 
calculateARD <- function(){
    #Only works for binary M, Y, and U right now
    if (!isBinary(fam[["Y"]]) || !isBinary(fam[["M"]]) || !isBinary(fam[["U"]])){
        stop("Only works for binary M, Y, and U right now")
    }
    
    #Only bother calculating for A = 1, since that is the covariate distribution 
    #we want to average over
    whichA1 <- which(A == 1)
    
    ###Calculate denominator, E[Y | A=0, Z=z, U=u]
    #Y design matrix for M=0
    XmatYifA0M0 <- Xmats[["Y"]][whichA1, ]
    XmatYifA0M0[ , grep("M", colnames(XmatYifA0M0))] <- 0 
    XmatYifA0M0[ , grep("A", colnames(XmatYifA0M0))] <- 0 
    
    #Y design matrix for M=1 (the ordering of M and A resets is important here!)
    XmatYifA0M1 <- Xmats[["Y"]][whichA1, ]
    XmatYifA0M1[ , grep("M", colnames(XmatYifA0M1))] <- 1
    XmatYifA0M1[ , grep("A", colnames(XmatYifA0M1))] <- 0 
    
    #M design matrix for A=0
    XmatMifA0 <- Xmats[["M"]][whichA1, ]
    XmatMifA0[ , grep("A", colnames(XmatMifA0))] <- 0 
    
    #Calculate P(Y=1) for these people with this new design matrix
    piYA0M0 <- getLinkinv(fam[["Y"]])(XmatYifA0M0 %*% Y.par)
    piYA0M1 <- getLinkinv(fam[["Y"]])(XmatYifA0M1 %*% Y.par)
    piMA0 <- getLinkinv(fam[["M"]])(XmatMifA0 %*% M.par)
    
    #Denominator: sum over M strata weighted by P(M=m | A=0, Z=z, U=u)
    denom <- piYA0M0 * (1-piMA0) + piYA0M1 * (piMA0)
    
    
    ###Calculate numerator, E[Y_Hx(0) | A=1, Z=z, U=u]
    #Y design matrix for M=0 (A is already set to 1)
    XmatYifA1M0 <- Xmats[["Y"]][whichA1, ]
    XmatYifA1M0[ , grep("M", colnames(XmatYifA0M0))] <- 0 
    
    #Y design matrix for M=1
    XmatYifA1M1 <- Xmats[["Y"]][whichA1, ]
    XmatYifA1M1[ , grep("M", colnames(XmatYifA0M1))] <- 1
    XmatYifA1M1[ , colnames(XmatYifA0M1) == "AM"] <- 1
    
    #Calculate counterfactual P(Y=1) for these people with this new design matrix
    piYA1M0 <- getLinkinv(fam[["Y"]])(XmatYifA1M0 %*% Y.par)
    piYA1M1 <- getLinkinv(fam[["Y"]])(XmatYifA1M1 %*% Y.par)
    
    #Numerator: sum over M strata weighted by P(M=m | A=0, Z=z, U=u)
    numer <- piYA1M0 * (1-piMA0) + piYA1M1 * (piMA0)

    ##Put it all together
    #Individual-level residual disparity
    disp <- numer/denom

    #Average residual disparity
    ARD <- mean(disp, na.rm = TRUE)
    
    #Return
    return(ARD)

}



