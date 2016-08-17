#' Make design matrix from formula and data frame
#'
#' @param formula R formula object
#' @param data Data frame containing all variables on RHS of formula
#' @return Matrix with appropriate column labels
#'
#' @export
make_Xmat <- function(formula, data){
    Xmat <- model.matrix(formula, nrow = nrow(data), data = data)
    coef_names <- colnames(Xmat)
    Xmat <- matrix(Xmat, nrow = nrow(data))
    colnames(Xmat) <- coef_names
    return(Xmat)
}
