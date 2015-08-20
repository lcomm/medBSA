#' Check whether a family indicates the data is binary (actually, binomial)
#' 
#' Returns TRUE if a the family is binomial and FALSE otherwise
#' 
#' @param family A family object
#' @return Logical TRUE or FALSE
#' 
#' @examples
#' ##Should be TRUE
#' isBinary(binomial(link = "logit"))
#' 
#' ##Should be FALSE
#' isBinary(multinomial(parallel = FALSE))
#' 
isBinary <- function(family){
    if (detectFamilyType(family) == "glm" && family[["family"]] == "binomial"){
        return(TRUE)
    } else {
        return(FALSE)
    }
}

