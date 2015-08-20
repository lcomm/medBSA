#' Extract list or element in list from props in the BSAe environment
#' 
#' This function pulls an element "v" from the list "props" stored in the BSAe
#' environment.
#' 
#' @param v The name of the element that should be returned; 
#' If v is not specified, the entire props list will be returned
#' 
#' @return The element of the version of props stored in the BSEe 
#' environment
#' 
getProp <- function(v = NULL){
    
    if (!is.null(v)){
        
        #Return selected element of Xmats
        return(fromBSAe("props", v))
        
    } else {
        
        #Return full Xmats object
        return(fromBSAe("props"))
        
    }
    
}

