#' Make a GLM-type variable based on the existing list values
#' 
#' This wrapper for genVar() and assumes there are existing lists fam, Xmats, and
#' params with element "v" that contain the necessary information.
#' 
#' @param v The string name of the variable to be generated
#' @param dimVar Only needed for categorical variable generation; number of unique
#' values the variable can take on
#' 
#' @seealso genVar
#' 
#' @examples
#' ##Lists that need to exist already
#' fam  = list(Y = binomial(link="logit"))
#' Xmats = list(Y = cbind(1, rnorm(10)))
#' params = list(Y = c(-1,1))
#' ##Make the variable based on these specifications
#' makeVar("Y")
#' 
makeVar = function(v, dimVar = NULL){
    
    #Categorical variable
    if (isMultinomialLogit(fam[[v]])){
        
        #Check to make sure dimVar was specified
        if (is.null(dimVar)){
            stop("Must specify dimVar (number of possible values) for multinomial variable creation!")
        }
        
        #Reshape parameter matrix into 
        parMat = matrix(params[[v]], 
                        nrow = length(params[[v]])/(dimVar-1), 
                        ncol = (dimVar-1), 
                        byrow = TRUE)
        
        #Get fitted probabilities based on the design matrix + parameter matrix
        probs = getLinkinv(fam[[v]])(Xmats[[v]] %*% parMat)
        
        #Generate based on multinomial logit
        gen.range = 0:(dimVar - 1)
        ans = apply(probs, 1, FUN = function(x) { 
                        sample(gen.range, size = 1, prob = x, replace=TRUE)
                    })
        
        #Return
        return(ans)
        
    } else if (detectFamilyType(fam[[v]]) == "glm"){
        
        #Traditional GLM
        ans = genVar(fam[[v]], Xmats[[v]], params[[v]])
        
        #Return
        return(ans)
        
    }
}
