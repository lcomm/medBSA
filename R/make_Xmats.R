#' Make design matrix from formula and data frame
#'
#' @param formula R formula object
#' @param data Data frame containing all variables on RHS of formula
#' @return Matrix with appropriate column labels
#'
#' @export
make_Xmat <- function(formula, data, ...){
    Xmat <- model.matrix(formula, nrow = nrow(data), data = data,
                         ...)
    coef_names <- colnames(Xmat)
    Xmat <- matrix(Xmat, nrow = nrow(data))
    colnames(Xmat) <- coef_names
    return(Xmat)
}


#' Make version of design matrix where some covariate is set to
#' some constant value
#'
#' Preserves dummy coding if variable to set is a factor
#'
#' @param formula R formula object for outcome model
#' @param data Data frame containing all variables on RHS of formula
#' @param varname Name of covariate to "set" to something
#' @param val Value covariate should be "set" to
#' @return Matrix with appropriate column labels
#'
#' @export
make_Xmat_set <- function(formula, data, varname, val){
    if (is.factor(data[[varname]])){
        xlev <- list(levels(data[[varname]]))
        names(xlev) <- varname
        data[[varname]] <- as.factor(val)
        return(make_Xmat(formula, data, xlev = xlev))
    } else {
        data[[varname]] <- val
        return(make_Xmat(formula, data))
    }
}


