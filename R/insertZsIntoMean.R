#' Insert zeros into a mean vector
#' 
#' This function takes a vector with named elements and inserts the desired
#' number of zeros into the middle of it. The zeros will be inserted after the 
#' last "Z#" and will be named "Z(# + 1)" through "Z(# + p)", where p is the 
#' number of zeros to insert.
#' 
#' @param vec Existing vector.  Elements must be named.
#' @param p Number of zeros to insert
#' 
#' @return A vector of p elements longer than the vector supplied
#' 
#' @examples
#' ##Make vector
#' myVec <- 1:10
#' names(myVec) <- c(LETTERS[1:4], paste0("Z", 1:3), LETTERS[7:9])
#' 
#' ##Insert two zeros into the vector after the last of the Z's
#' insertZsIntoMean(myVec, 2) 
#' 
#' @seealso insertZsIntoVcov
#' 
insertZsIntoMean <- function(vec, p){

    #Parameter checking
    if (!is.vector(vec)){
        stop("Need vector as input!")
    } else if (is.null(names(vec))){
        stop("Need element names!")
    } else if (identical(grep("Z", names(vec)), integer(0))) {
        stop("Need names to include Z")
    } 
    
    #Figure out length of the vector
    dimVec = length(vec)
    
    #We insert the columns after the last Z# column
    whichLastZ  <- max(grep("Z", names(vec)))
    nOldZ       <- length(grep("Z", names(vec)))
    
    #Figure out where to split the existing matrix and how many columns go before/after
    nBefore <- whichLastZ
    
    #Make new vector
    newVec <- c(vec[1:nBefore], rep(0, p), vec[(nBefore + 1):dimVec])
    
    #Apply names
    newZnames <- paste0("Z", (nOldZ + 1):(nOldZ + p))
    names(newVec) <- c(names(vec)[1:nBefore], 
                       newZnames, 
                       names(vec)[(nBefore + 1):dimVec])
    
    #Return constructed vector
    return(newVec)

}

