#' Check whether the family is multinomial logit
#' 
#' This function returns TRUE if the family is multinomial with a logit
#' link and FALSE otherwise
#' 
#' @param family A family object
#' @return Logical TRUE or FALSE
#' 
#' @examples
#' ##Should be TRUE
#' isMultinomialLogit(family = multinomial(parallel=FALSE))
#' 
#' ##Should be FALSE
#' isMultinomialLogit(family = binomial(link="logit"))
#' 
isMultinomialLogit <- function(family){
    if (class(family) == "vglmff" 
        && 
            (grep("Multinomial logit", attr(family, "blurb")[1], ignore.case = TRUE) == 1)){
        #Return
        return(TRUE)
    } else {
        return(FALSE)
    }
    
}

