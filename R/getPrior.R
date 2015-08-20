#' Extract list or element in list from priors in the BSAe environment
#' 
#' This function pulls an element "v" from the list "priors" stored in the BSAe
#' environment.
#' 
#' @param v The name of the element that should be returned; 
#' If v is not specified, the entire priors list will be returned
#' 
#' @return The element of the version of priors stored in the BSEe 
#' environment
#' 
getPrior <- function(v = NULL){
    
    if (!is.null(v)){
        
        #Return selected element of Xmats
        return(fromBSAe("priors", v))
        
    } else {
        
        #Return full Xmats object
        return(fromBSAe("priors"))
        
    }
    
}

