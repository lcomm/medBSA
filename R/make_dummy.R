#' Make dummy variable matrix
#'
#' Reference level is always the first level
#'
#' @param vec Vector containing factor
#' @return A matrix of dummy variables
#'
#' @export
make_dummy <- function(vec){
    if (!is.integer(vec)){
        stop("Must be integer valued")
    }
    matrix(model.matrix(~ as.factor(vec)), nrow = length(vec))[,-1]
}

