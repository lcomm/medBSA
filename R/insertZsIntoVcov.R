#' Insert rows/columns with offdiagonal zeros into an existing matrix
#' 
#' This functions takes a square matrix and adds columns and rows corresponding
#' to additional Z variables.  Off-diagonal elements in the added rows and 
#' columns will be 0, and diagonal elements will be whatever is passed in via 
#' the "diagElem" parameter.  The columns/rows will be inserted after the last
#' "Z#" and will be named "Z(# + 1)" through "Z(# + p)", where p is the number
#' of columns and rows to insert.
#' 
#' @param mat Existing variance-covariance matrix.  Columns must be named.
#' @param diagElem Scalar or vector containing the diagonal entries for the
#' inserted rows/columns.  If the length of diag is 1 and less than p, then
#' diagElem will be the diagonal element for every inserted row/column.
#' @param p Number of rows and columns to insert. If not specified, then the 
#' default is to use the length of diagElem
#' 
#' @return A square matrix of dimension p greater than the matrix supplied
#' 
#' @examples
#' ##Make an matrix with the right column names
#' myMat <- matrix(1:25, nrow = 5, ncol = 5)
#' colnames(myMat) <- c("A", "Z1", "Z2", "U", "M")
#' 
#' ##Insert 3 new columns/rows, with diagonal entries of 100, 200, and 300
#' insertZsIntoVcov(myMat, diagElem = c(100, 200, 300))
#' insertZsIntoVcov(myMat, diagElem = 100, p = 4)

#' 
insertZsIntoVcov <- function(mat, diagElem, p = length(diagElem)){

    #Parameter checking
    if (!is.matrix(mat) || nrow(mat) != ncol(mat)){
        stop("Need square matrix as input!")
    } else if (is.null(colnames(mat))){
        stop("Need column names to insert rows/columns!")
    } else if (identical(grep("Z", colnames(mat)), integer(0))) {
        stop("Need Z columns in matrix")
    } 
    
    #Figure out if we need to duplicate diagElem to become a vector of the right length
    if ((p > 1) & (length(diagElem) == 1)) {
        diagElem <- rep(diagElem, p)
    }
    
    #Figure out dimensions of the matrix
    dimMat = nrow(mat)
    
    #We insert the columns after the last Z# column
    whichLastZ  <- max(grep("Z", colnames(mat)))
    nOldZ       <- length(grep("Z", colnames(mat)))
    
    #Figure out where to split the existing matrix and how many columns go before/after
    splitHere <- nColBefore <- whichLastZ
    nColAfter <- dimMat - nColBefore
    
    #Extract components from existing matrix
    upLeftBlock   <- mat[1:nColBefore, 1:nColBefore]
    loRightBlock  <- mat[(nColBefore + 1):dimMat, (nColBefore + 1):dimMat]
    rightBlock    <- mat[1:nColBefore, (nColBefore + 1):dimMat]
    leftBlock     <- mat[(nColBefore + 1):dimMat, 1:nColBefore]
    
    #Make new matrix with correct diagonal elements in the new rows/columns
    newMat <- diag(c(rep(0,nColBefore), diagElem, rep(0,nColAfter)))
    
    #Put the block pieces into the new matrix
    dimNewMat <- dimMat + p
    newMat[1:nColBefore, 1:nColBefore] <- upLeftBlock
    newMat[(nColBefore + 1 + p):dimNewMat, (nColBefore + 1 + p):dimNewMat] <- loRightBlock
    newMat[1:nColBefore, (nColBefore + 1 + p):dimNewMat] <- rightBlock
    newMat[(nColBefore + 1 + p):dimNewMat, 1:nColBefore] <- leftBlock
    
    #Name columns
    colnames(newMat)[c(1:nColBefore, (nColBefore + 1 + p):dimNewMat)] <- colnames(mat)
    newZnames <- paste0("Z", (nOldZ + 1):(nOldZ + p))
    colnames(newMat)[(nColBefore+1):(nColBefore + p)] <- newZnames
    
    #Name rows as well if the old matrix had row names and they were same as colnames
    if (!is.null(rownames(mat)) && identical(rownames(mat), colnames(mat))) { 
        rownames(newMat) <- colnames(newMat)
    }
    
    #Return constructed matrix
    return(newMat)

}


