#' Update design matrices in BSAe environment with new value of U
#' 
#' After a new U vector is imputed, this updates any matrices stored in the 
#' Xmats list to the new value of U (uses the fact that the design matrix columns
#' are labelled).
#' 
#' @seealso get.pU1
#' 
updateXmats <- function(){
    #Update U, if necessary, in every Xmat matrix with U as a column
    for (i in 1:length(BSAe$Xmats)){
        if ("U" %in% colnames(BSAe$Xmats[[i]])){
            whichCol = which(colnames(BSAe$Xmats[[i]]) == "U")
            BSAe$Xmats[[names(BSAe$Xmats)[i]]][ ,whichCol] <<- BSAe$U
        }
    }
}

