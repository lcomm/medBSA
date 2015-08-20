#' Extract list or element in list from BSAe environment
#' 
#' This function pulls an element "v" from the list "name" stored in the BSAe
#' environment.
#' 
#' @param v The name of the element that should be returned; 
#' If v is not specified, the entire list will be returned
#' 
#' @return The  element of the version of Xmats stored in the BSEe 
#' environment
#' 
#' 
#' @examples
#' BSAe <- new.env()
#' BSAe$params = list(Y = "this is what I want to get")
#' fromBSAe("params", "Y")
#' 
fromBSAe <- function(name, v = NULL){
    
    if (!is.null(v)){
        
        #Return selected element of Xmats
        return(get(name, envir = BSAe)[[v]])
        
    } else {
        
        #Return full Xmats object
        return(get(name, envir = BSAe))
        
    }
    
}

