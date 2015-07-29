#' Make a GLM-type variable based on the existing list values
#' 
#' This wrapper for genVar() and assumes there are existing lists fam, Xmats, and
#' params with element "v" that contain the necessary information.
#' 
#' @param v The string name of the variable to be generated
#' @seealso genVar
#' @examples
#' ##Lists that need to exist already
#' fam  = list(Y = binomial(link="logit"))
#' Xmats = list(Y = cbind(1, rnorm(10)))
#' params = list(Y = c(-1,1))
#' ##Make the variable based on these specifications
#' makeVar("Y")
#' 
makeVar = function(v){
    genVar(fam[[v]], Xmats[[v]], params[[v]])
}
