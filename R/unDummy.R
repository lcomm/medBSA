#' Collapse matrix of dummy variables into a single vector-valued version
#' 
#' Reference category will be 0.
#' 
#' @param mat Matrix of dummy variables (may or may not contain reference indicator)
#' 
#' @examples
#' ##Version where all rows sum to 1 (i.e., reference column has been left in)
#' dummy.mat1 <- rbind(c(0,0,0,1), c(0,0,1,0), c(1,0,0,0))
#' unDummy(dummy.mat1)
#' 
#' ##Version where some rows are all zeros (i.e., reference column removed)
#' dummy.mat2 <- rbind(c(0,0,1), c(0,1,0), c(0,0,0))
#' unDummy(dummy.mat2)
#' 
unDummy = function(mat){
    
    #Reference dummy still there
    if (identical(rowSums(mat), rep(1, nrow(mat)))) {
        apply(mat, 1, FUN = function(x) { which(as.logical(x)) - 1 } )
    } else {
        #Reference column has been removed
        apply(mat, 1, FUN=function(x) { if (sum(x)> 0) {which(as.logical(x));} else{0} } )
    }
    
}

