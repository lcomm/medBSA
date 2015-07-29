#' Update design matrices after with new value of U
#' 
#' After a new U vector is imputed, this updates any matrices stored in the 
#' Xmats list to the new value of U (uses the fact that the design matrix columns
#' are labelled).
#' 
#' @seealso get.pU1
#' 
updateXmats = function(){
    #Update U, if necessary, in every Xmat matrix with U as a column
    for (i in 1:length(Xmats)){
        if ("U" %in% colnames(Xmats[[i]])){
            whichCol = which(colnames(Xmats[[i]]) == "U")
            Xmats[[names(Xmats)[i]]][,whichCol] <<- U
        }
    }
}

