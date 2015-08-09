#' Draw a parameter from the asymptotic distribution of its MLE
#' 
#' It relies on the fact that there is an existing list in the environment
#' called "Xmats" that has a sublist with the variable's name and that sublist 
#' contains the design matrix for the regression model where "V" is the outcome.
#' 
#' It also assumes that there is a similarly structured list called "fam" where the
#' family of "v" is stored, and a list called "params" that stores decent starting 
#' values for the GLM (to assist in convergence).
#' 
#' It updates the "props" list with the new proposal mean and variance based on the
#' asymptotic distribution of the MLE from the regression fit
#' 
#' @param v The name of the variable, as a string
#' @return A parameter vector drawn from the asymptotic distribution of the fitted MLE 
#' (if it converged) or the multivariate normal prior (if it did not)
#' @seealso n.rmvnorm, drawParam
#' @examples
#' ##Make the lists that need to already be in the environment
#' props = list(Y = list(mean = c(0,0), sigma = diag(c(1,1))))
#' params = list(Y = c(0,0))
#' fam = list(Y = binomial(link="logit"))
#' Xmats = list(Y = cbind(1, rep(0:1, each = 5)))
#' 
#' ##Make the outcome variable
#' Y = rbinom(n = 10, size = 1, prob = 0.5)
#' 
#' ##Draw a vector from the prior
#' drawParamFromPrior(v = "Y")
#' 
drawParamFromMLE <- function(v){
    
    #Extract variable
    outcome = get(v)
    while (is.character(outcome)) { outcome = get(outcome) }
    
    #Fit frequentist GLM unless VGLM is specified
    if (detectFamilyType(fam[[v]]) == "glm"){
        MLfit = suppressWarnings(glm(outcome ~ as.matrix(Xmats[[v]]) - 1,
                                     family = fam[[v]],
                                     start = params[[v]]))
        
        #Report back if having convergence or multicollinearity problems
        if (MLfit[["converged"]] == FALSE | (length(coef(MLfit)) != length(params[[v]]))) {
            #Let us know it was a problem
            print("Convergence issues for MLE!")
            #Use priors
            props[[v]][["mean"]] <<- priors[[v]][["mean"]]
            props[[v]][["Sigma"]] <<- priors[[v]][["Sigma"]]
        } else if (MLfit[["converged"]] == TRUE) {
            #Save proposal information
            props[[v]][["mean"]] <<- coef(MLfit)
            props[[v]][["Sigma"]] <<- vcov(MLfit)
        }
    } else if (detectFamilyType(fam[[v]]) == "multinom"){
        #Fit multinomial model
        MLfit = vglm(outcome ~ as.matrix(Xmats[[v]]) - 1,
                     family = fam[[v]])
        #Save proposal information
        props[[v]][["mean"]] <<- coef(MLfit)
        props[[v]][["Sigma"]] <<- vcov(MLfit)
    }
    #Draw parameter vector
    return(drawParam(v))
    
}

