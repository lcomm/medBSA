#' Propose new values for a coefficient vector or matrix from an MVN
#'
#' @param coef Current value of coefficient vector or matrix
#' @param prop_mean Mean of MVM proposal distribution. Defaults to current
#' value of coefficient vector (i.e., random vector)
#' @param prop_Sigma Variance-covariance matrix of MVM proposal distribution
#'
#' @return Vector or matrix of same shape as coef
#' @export
propose_coef <- function(coef, prop_mean = c(coef), prop_Sigma){

    #Copy structure of existing coefficient container
    coef_star <- coef

    #Propose from MVN with correct mean and variance
    coef_star[] <- c(rmvn(1, prop_mean, prop_Sigma))

    #Return
    return(coef_star)
}

#' Calculate log-acceptance ratio for coefficient vectors or matrices
#'
#' @param data Data frame
#' @param outcome Character containing outcome model variable name (e.g., "Y")
#' @param Xmat Design matrix corresponding to coefficient vector or matrix
#' @param coef Current value of coefficient vector or matrix
#' @param coef_star Proposed value of coefficient vector or matrix
#' @param prior_mean Prior distribution mean vector
#' @param prior_Sigma Prior distribution variance-covariance matrix
#' @param prop_Sigma Proposal distribution variance-covariance matrix
#'
#' @return Scalar log-acceptance ratio
#' @export
calc_logAR <- function(data, outcome, Xmat,
                       coef, coef_star,
                       prior_mean = rep(0, length(coef)),
                       prior_Sigma = diag(length(coef))*100,
                       prop_Sigma = diag(length(coef))){

    #Difference in log-likelihoods (depends on outcome model form)
    diff_ll <- switch(outcome,
                      "Y" = sum(medBSA:::ll_Y(data$Y, Xmat, coef_star)) -
                          sum(medBSA:::ll_Y(data$Y, Xmat, coef)),
                      "U" = sum(medBSA:::ll_U(data$U, Xmat, coef_star)) -
                          sum(medBSA:::ll_U(data$U, Xmat, coef)),
                      "M" = sum(medBSA:::ll_M(data$M, Xmat, coef_star)) -
                          sum(medBSA:::ll_M(data$M, Xmat, coef)))

    #Difference in log-prior densities
    diff_prior <- dmvn(c(coef_star), prior_mean, prior_Sigma, log = TRUE) -
        dmvn(c(coef), prior_mean, prior_Sigma, log = TRUE)

    #Difference in proposal densities (not relevant for random walk...)
    diff_prop <- dmvn(c(coef_star), c(coef), prop_Sigma, log = TRUE) -
        dmvn(c(coef), c(coef_star), prop_Sigma, log = TRUE)

    #Put together to return log-acceptance ratio
    return(diff_ll + diff_prior - diff_prop)

}
