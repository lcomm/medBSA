#' Return the posterior probability of U equalling 1 for each observation
#' 
#' This function is used in the imputation of U.  It calculates P(U=1|everything)
#' from the posterior.  This formula depends on link functions, and requires the
#' lists Xmats, as well as the string variable case (mediator or not) to exist in the environment.
#' 
#' @return A length-n vector of posterior P(U = 1)
#' @seealso Like
#' 
get.pU1 = function(){
    
    #Define design matrix for Y where U's are all 1 or 0
    XmatYU1 = XmatYU0 = Xmats[["Y"]]
    XmatYU1[, colnames(Xmats[["Y"]]) == "U"] = 1
    XmatYU0[, colnames(Xmats[["Y"]]) == "U"] = 0
    
    #If applicable, define design matrix for M where U's are all 1 or 0
    if (case == "mediator"){
        XmatMU1 = XmatMU0 = Xmats[["M"]]
        XmatMU1[, colnames(Xmats[["M"]]) == "U"] = 1
        XmatMU0[, colnames(Xmats[["M"]]) == "U"] = 0
    }
    
    #Calculate P(U=1) based on link for U regression
    if (case == "mediator"){
        #Calculate piU, the P(U=1) from U regression part
        piU = fam[["U"]][["linkinv"]](Xmats[["U"]] %*% U.par)
        
        #Un-normalized P(U_i = 1 | everything)
        logPieceA = Like("Y", Xmat = XmatYU1, par = Y.par, log = TRUE, summation = FALSE) +
                    Like("M", Xmat = XmatMU1, par = M.par, log = TRUE, summation = FALSE) + 
                    log(piU)
        
        #Un-normalized P(U_i = 0 | everything)
        logPieceB = Like("Y", Xmat = XmatYU0, par = Y.par, log = TRUE, summation = FALSE) +
                    Like("M", Xmat = XmatMU0, par = M.par, log = TRUE, summation = FALSE) + 
                    log(1 - piU)
        
        #Normalized vector of P(U_i = 1 | everything)
        pU1 = 1/(1 + exp(logPieceB - logPieceA))
        
        #Approach 2
        #Commenting out for now, but could be useful for overflow/underflow?
#         Ydiff =  Like("Y", Xmat = XmatYU0, par = Y.par, log = TRUE) -
#                  Like("Y", Xmat = XmatYU1, par = Y.par, log = TRUE)
#         Mdiff =  Like("M", Xmat = XmatMU0, par = M.par, log = TRUE) -
#                  Like("M", Xmat = XmatMU1, par = M.par, log = TRUE)
#         piUdiff = log(1-piU) - log(piU) 
#         1/(1 + exp(Ydiff + Mdiff + piUdiff))
        
    } else {        
        #Calculate piU, the P(U=1) from U regression part
        piU = fam[["U"]][["linkinv"]](Xmats[["U"]] %*% U.par)
        
        #Un-normalized P(U_i = 1 | everything)
        logPieceA = Like("Y", Xmat = XmatYU1, log = TRUE, summation = FALSE) + 
                    log(piU)
        
        #Un-normalized P(U_i = 0 | everything)
        logPieceB = Like("Y", Xmat = XmatYU0, log = TRUE, summation = FALSE) + 
                    log(1-piU)
        
        #Normalized vector of P(U_i = 1 | everything)
        pU1 = 1/(1 + exp(logPieceB - logPieceA))
    }
    
    #Return P(U=1) to be used in imputation
    return(pU1)
}


