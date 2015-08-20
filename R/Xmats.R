#' Extract list or element in list from Xmats in the BSAe environment
#' 
#' This function pulls an element "v" from the list "Xmats" stored in the BSAe
#' environment.
#' 
#' @param v The name of the element that should be returned; 
#' If v is not specified, the entire Xmats list will be returned
#' 
#' @return The element of the version of Xmats stored in the BSEe 
#' environment
#' 
Xmats <- function(v = NULL){
    
    if (!is.null(v)){
        
        #Return selected element of Xmats
        return(fromBSAe("Xmats", v))
        
    } else {
        
        #Return full Xmats object
        return(fromBSAe("Xmats"))
        
    }
    
}

