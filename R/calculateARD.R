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
#' @param Yval The value of Y for which to evaluate the risk ratio.  
#' Defaults to 1, so returned risk ratio will be for P(Y=1)
#' 
#' @return ARD The average residual disparity after intervening on M
#' 
calculateARD <- function(Yval = 1){
    #Only works for binary Y, and U right now
    if (!isBinary(fam[["Y"]]) || !isBinary(fam[["U"]])){
        stop("Only works for binary Y, and U right now")
    }
    
    #Only bother calculating for A = 1, since that is the covariate distribution 
    #we want to average over
    whichA1 <- which(A == 1)

    #M design matrix for A=0
    XmatMifA0 <- Xmats[["M"]][whichA1, ]
    XmatMifA0[ , grep("A", colnames(XmatMifA0))] <- 0 
    
    
    #Split based on whether M is binary or multinomial
    if (isBinary(fam[["M"]])){
        ###Calculate denominator, E[Y | A=0, Z=z, U=u]
        #Y design matrix for M=0
        XmatYifA0M0 <- Xmats[["Y"]][whichA1, ]
        XmatYifA0M0[ , grep("M", colnames(XmatYifA0M0))] <- 0 
        XmatYifA0M0[ , grep("A", colnames(XmatYifA0M0))] <- 0 
        
        #Y design matrix for M=1 (the ordering of M and A resets is important here!)
        XmatYifA0M1 <- Xmats[["Y"]][whichA1, ]
        XmatYifA0M1[ , grep("M", colnames(XmatYifA0M1))] <- 1
        XmatYifA0M1[ , grep("A", colnames(XmatYifA0M1))] <- 0 
        
        #Calculate P(Y=1) for these people with this new design matrix
        piYA0M0 <- getLinkinv(fam[["Y"]])(XmatYifA0M0 %*% Y.par)
        piYA0M1 <- getLinkinv(fam[["Y"]])(XmatYifA0M1 %*% Y.par)
        piMA0 <- getLinkinv(fam[["M"]])(XmatMifA0 %*% M.par)
        
        #Denominator: sum over M strata weighted by P(M=m | A=0, Z=z, U=u)
        if (Yval == 1) {
            #Just usual formula
            denom <- piYA0M0 * (1-piMA0) + piYA0M1 * (piMA0)
        } else if (Yval == 0){
            #Have to flip because we actually wanted P(Y=0)
            denom <- (1-piYA0M0) * (1-piMA0) + (1-piYA0M1) * (piMA0)
        }
        
        
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
        if (Yval == 1) {
            #Just usual formula
            numer <- piYA1M0 * (1-piMA0) + piYA1M1 * (piMA0)
        } else if (Yval == 0){
            #Have to flip because we actually wanted P(Y=0)
            numer <- (1-piYA1M0) * (1-piMA0) + (1-piYA1M1) * (piMA0)
        }
        
    #End binary M case
    
    } else if (isMultinomialLogit(fam[["M"]])){
        
        #Reshape M.par into a form suitable for applying inverse multilogit function
        #Copy over
        par <- M.par
        
        #### Y regression
        #Extract columns having to do with interaction variable
        col_intx <- grep("AM", colnames(Xmats[["Y"]]))
        
        #Extract columns having to do with M variable at all
        col_any_M <- grep("M", colnames(Xmats[["Y"]]))
        
        #Non-interaction M variables
        col_M <- col_any_M[!(col_any_M %in% col_intx)]
        
        #Extract columns constant regardless of M
        col_other <- (1:ncol(Xmats[["Y"]]))[!(1:ncol(Xmats[["Y"]]) %in% col_any_M)]
        Ymat_o <- Xmats[["Y"]][whichA1, col_other]
        Ypar_o <- Y.par[col_other]
        
        #Extract Y design matrix only for people with A = 1
        XmatYA0 <- Xmats[["Y"]][whichA1, ]
        
        #Change A column of M design matrix to be 0
        col_A_XmatY <- grep("A", colnames(XmatYA0))
        XmatYA0[, col_A_XmatY] <- 0
        
        
        #### M regression
        #Get M regression parameters into matrix form for multilogit inversion
        par <- matrix(M.par, nrow=ncol(M) - 1)
        
        #Extract M design matrix only for people with A = 1
        XmatMA0 <- Xmats[["M"]][whichA1, ]
        
        #Change A column of M design matrix to be 0
        col_A_XmatM <- grep("A", colnames(XmatMA0))
        XmatMA0[, col_A_XmatM] <- 0
        
        #Calculate probabilities of each (non-zero) M value had A been 0
        probs <- matrix(multilogit(multilogit(XmatMA0 %*% par, inverse = TRUE)), ncol=3, byrow=TRUE)
        
        #Get probability for M being 0
        probs0 <- 1 - rowSums(probs)
        
        #Evaluate counterfactuals
        for (m in 0:length(col_M)){
            if (m == 0) {
                #Initialize 
                numer <- denom <- 0
                
                #Linear predictor A = 0 is just non-M-related linear predictor
                #Not including m terms since m = 0 here
                etaA0 <- Ymat_o %*% Ypar_o
                EYA0 <- getLinkinv(fam[["Y"]])(etaA0)
                
                #Linear predictor for A = 1 includes A parameter(s)
                #Not including m terms since m = 0 here
                etaA1 <- Ymat_o %*% Ypar_o + Y.par[col_A_XmatY]
                EYA1 <- getLinkinv(fam[["Y"]])(etaA1)
                
                #Numerator is for disadvantaged group
                #Using probs0 because m = 0
                numer <- EYA1 * probs0
                
                #Denominator is as if A = 0
                #Using probs0 because m = 0
                denom <- EYA0 * probs0
                
            } else {
                
                #Linear predictor A = 0 is just non-A-related linear predictor
                etaA0 <- Ymat_o %*% Ypar_o + Y.par[col_M[m]]
                etaA1 <- Ymat_o %*% Ypar_o + Y.par[col_M[m]] + Y.par[col_A_XmatY]
                
                #Add interaction term if necessary
                #Don't need to worry about interaction for A = 0
                if (intx == "yes") {
                    etaA1 <- etaA1 + Y.par[col_intx[m]]
                }
                
                #Expected values
                EYA0 <- getLinkinv(fam[["Y"]])(etaA0)
                EYA1 <- getLinkinv(fam[["Y"]])(etaA1)
                
                #Numerator is for disadvantaged group (A = 1)
                #Using probs0 because m = 0
                numer <- numer + EYA1 * probs[,m]
                
                #Denominator is as if A = 0
                denom <- denom + EYA0 * probs[,m]
                
            } #End branching based on m value
            
        } #End summing over components of counterfactual
        
    } #End branching based on binary/categorical

    ##Put it all together
    #Individual-level residual disparity
    disp <- numer/denom
    

    #Average residual disparity
    ARD <- mean(disp, na.rm = TRUE)
    
    #Return
    return(ARD)

}
