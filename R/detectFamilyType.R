#' Detect type of family
#' 
#' Returns whether the family passed is belongs fo GLM or VGLM multinomial
#' 
#' @param family Family object to be parsed
#' @return Character vector of "glm" or "multinom"
#' 
#' @examples
#' ##GLM
#' detectFamilyType(binomial(link="logit"))
#' 
#' ##VGLM
#' detectFamilyType(multinomial(parallel=TRUE))
#' 
detectFamilyType <- function(family){
    switch(class(family),
           "family" = "glm",
           "vglmff" = "multinom")
}

