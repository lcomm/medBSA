#' Generate a random variable based on GLM framework
#' 
#' This function generates a random variable if passed an X matrix, a link, 
#' and a parameter set. For now, only Bernoulli random variables are supported.
#' 
#' @param fam The family of the outcome, as would be passed to the GLM fit 
#' (like "binomial(link="logit")")
#' @param Xmat A design X matrix
#' @param par The vector of regression coefficients
#' @seealso rbern
#' @examples
#' genVar(fam = binomial(link="logit"), Xmat = cbind(1, rnorm(10)), par = c(-1,1))
#' 
genVar <- function(fam, Xmat, par){
    #Verify conformability for matrix multiplication
    if (detectFamilyType(fam) == "glm" & ncol(Xmat) != length(par)) {
        stop("GENVAR: design matrix and parameter vector nonconformable")
    }
    
    #Make variable based on what is requested
    if (fam$family == "binomial" & fam$link == "logit"){
        rbern(fam[["linkinv"]](Xmat %*% par))
    } else if (fam$family == "binomial" & fam$link == "log"){
        if (max(Xmat %*% par) > 0){
            stop("GENVAR: probabilities above 1 for log link!")
        } else { 
            rbern(fam[["linkinv"]](Xmat %*% par))
        }
    } else {
        stop("GENVAR: Unsupported link/variable family combination!")
    }
}

