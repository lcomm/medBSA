#' Fit a frequentist GLM or VGLM
#' 
#' Given a variable string name v, this function fits a GLM or VGLM based on the
#' design matrix found in the Xmats list Xmats[[v]] and the family found in the 
#' fam list in the larger environment
#' 
#' @param v The name of the variable to use as the outcome in the regression
#' 
#' @examples
#' ##Set up lists in the environment
#' Xmats    <- list(Y = cbind(1, rep(1:5, each = 3)))
#' fam      <- list(Y = binomial(link="logit"))
#' params   <- list(Y = c(0,0))
#' 
#' ##Make outcome variable with regression coefficient vector c(0,0.2)
#' Y <- rbinom(n = 15, size = 1, prob = 1/(1+exp(-X %*% c(0,0.2))))
#' 
#' ##Call the function
#' summary(fitFrequentistModel("Y"))
fitFrequentistModel <- function(v){
    
    #Get outcome variable
    outcome = get("v")
    
    #Evaluate another time if still character (means we are more nested)
    while (is.character(outcome)) { outcome = get(outcome) }
    
    #Fit multinomial logit model if that is indicated
    #Otherwise, fit GLM
    if (class(fam[[v]]) == "vglmff" && attr(fam[[v]], "vfamily") == "multinomial"){
        #Fit multinomial logit model
        fit <- vglm(outcome ~ -1 + Xmat, family = fam[[v]])
    } else {
        #Fit
        fit <- glm(outcome ~ -1 + as.matrix(Xmats[[v]]), family = fam[[v]], start = params[[v]])
    }
    
    #Return the fit
    return(fit)
    
}

