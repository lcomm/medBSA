#' Extract link function from a family object
#' 
#' Returns a function corresponding to the desired link
#' 
#' @param family A family object
#' @return A function with the appropriate link
#' 
#' @seealso detectFamilyType
#' 
#' @examples
#' getLinkfun(binomial(link="logit"))
#' getLinkfun(multinomial(parallel=FALSE))
#' 
getLinkfun <- function(family){
    switch(detectFamilyType(family),
           "glm" = family[["linkfun"]],
           "multinom" = attr(family, "linkfun"))
}

