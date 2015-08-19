#' Rename columns and rows of a square matrix
#' 
#' @param mat Square matrix in need of having column and row renamed
#' @param wantName New name for column and row that will get changed
#' @param haveName Current column name for the row/column of interest
#' 
renameColsRows <- function(mat, wantName, haveName){
    #Parameter check
    if (!(haveName %in% colnames(mat))) { stop("Not a column name") }
    
    #Extract which column/row has that name
    whichCol = which(colnames(mat) == haveName)
    
    #Reassign names
    rownames(mat)[whichCol] = wantName
    colnames(mat)[whichCol] = wantName
    
    #Return
    return(mat)
    
}

