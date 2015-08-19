#' Reorder the rows and columns of a square matrix using column names
#' 
#' Returns a matrix with columns and rows switched so that the columns (and
#' analogous rows) are in the desired order
#' 
#' @param mat Matrix with original column and row orderings
#' @param wantOrder Character list containing the desired order of column names
#' 
reorderRowsCols <- function(mat, wantOrder){
    #Reorder the columns
    mat = mat[,wantOrder]
    
    #Then reorder the rows
    mat = mat[wantOrder,]
    
    #Return
    return(mat)
    
}
