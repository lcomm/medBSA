#' Assign a value to a name in the BSAe environment
#' 
#' This function is a shortcut way of assigning in the BSAe environment.  All 
#' parameters and options are passed to the "assign" function.
#'  
#' @examples
#' ## Initialize environment
#' initializeBSAe()
#' 
#' ## Assign a vector to the name "Y" inside the environment
#' assignBSAe("Y", 1:10)
#' 
#' ## Print
#' print(BSAe$Y)
#' 
assignBSAe <- function(...){
    
    #Assign
    assign(..., envir = BSAe)
    
}


