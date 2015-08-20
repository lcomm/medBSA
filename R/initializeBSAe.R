#' Initialize BSAe environment for medBSA functions
#' 
#' This function starts a fresh environment called "BSAe" which is used in many
#' functions relating to the BSA for mediation.  If "quietly" is set to FALSE, 
#' it lets the user know if an environment of the same name has been overwitten.
#' 
#' @param quietly Should the user be notified if an environment called BSEe 
#' already exists? Defaults to FALSE, meaning that notification is given
#' 
#' @examples
#' initializeBSAe()
#'
initializeBSAe <- function(quietly = FALSE){
    
    #Check if environment already exists
    if (exists("BSAe", envir = .GlobalEnv) && is.environment(BSAe)){
        
        #Notify user environment is being overwritten
        if (!quietly) cat("Starting fresh with new BSAe environment... \n")
        
        #Remove environment
        rm(BSAe)
        
    } 
        
    #Make new environment named BSAe
    BSAe <<- new.env(parent = .GlobalEnv)
    
}

