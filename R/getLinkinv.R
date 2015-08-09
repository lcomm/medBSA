#' Extract inverse link function from a family object
#' 
#' Returns a function corresponding to the desired inverse link
#' 
#' @param family A family object
#' @return A function with the appropriate link-inverse
#' 
#' @seealso detectFamilyType
#' 
#' @examples
#' getLinkinv(binomial(link="logit"))
#' getLinkinv(multinomial(parallel=FALSE))
#' 
getLinkinv <- function(family){
    switch(detectFamilyType(family),
           "glm" = family[["linkinv"]],
           "multinom" = attr(family, "linkinv"))
}


