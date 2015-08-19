#' Expand a vector of integer values to the dummy variable version
#' 
#' This function takes an integer vector and turns.  The lowest number is assumed to be
#' the reference level and the indicator variable column for this level
#' will be dropped from the matrix if dropRef is set to TRUE.
#' 
#' @param vec Vector to be turned into indicator matrix
#' @param ref Reference category; if none specified, defaults to the lowest level
#' @param dropRef Should the reference column be dropped? Defaults to FALSE
#' @param prefix Prefix for column labels in returned matrix
#' 
#' @return A matrix of indicator values for each of the levels
#' 
makeMultinomialDummy <- function(vec, ref = 1, dropRef = FALSE, prefix = NULL){
    
    #Get levels 
    nLevs = length(unique(vec))
        
    #Turn into design matrix
    desMat = t(sapply(as.factor(vec), 
                      FUN=function(x){ 
                          ans = rep(0, nLevs)
                          ans[x] = 1
                          return(ans) 
                      }))
    
    #Ensure it's a matrix not just a vector
    if (nLevs == 2){
        desMat = as.matrix(desMat, ncol = 2, nrow = length(vec))
    }
    
    #Apply column labels
    if (!is.null(prefix)) { 
        if (nLevs > 2){
            colnames(desMat) = paste0(prefix, 0:(nLevs-1))
        } else {
            colnames(desMat) = c("Ref", prefix)    
        }
    } else {
        colnames(desMat) = 0:(nLevs-1)
    }
    
    #Return requested value
    if (dropRef == TRUE) {
        desMatSmall = as.matrix(desMat[,-ref])
        colnames(desMatSmall) = colnames(desMat)[-ref]
        return(desMatSmall)
    } else {
        return(desMat)
    }

}

