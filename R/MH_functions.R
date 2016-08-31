#' Independence sampler updating with fixed distribution
#' Proposes from a MVN with mean prop_mean and
#' variance-covariance matrix prop_Sigma
#'
#' Supports split block updating
#'
#' @param coef Coefficient vector or matrix
#' @param outcome Character containing outcome variable name in data
#' @param data Data frame
#' @param Xmat Design matrix for regression
#' @param ll_func Function to calculate log-likelihood for model
#' @param dprior Function to evaluate prior density for coefficients
#' @param prop_mean Mean of proposal distribution
#' @param prop_Sigma Variance-covariance matrix for proposal distribution
#' @param block Vector or matrix of same dimension of coef, denoting with which
#' block the parameter should be updated. Defaults to update all parameters at
#' once (all block = 1).
#'
#' @return List containing updated coefficient (coef) and vector or matrix of
#' whether updates were accepted
#' @export
indep <- function(coef, outcome,
                  data, Xmat,
                  ll_func, dprior,
                  prop_mean, prop_Sigma,
                  block = coef*0 + 1){

    #Keep track of acceptances for each parameter
    acc <- coef*0

    #Loop over the blocks
    for (i in 1:max(block)){
        set <- which(block == i)

        #Restrict proposal mean and variance
        res_mean <- prop_mean[set]
        res_Sigma <- prop_Sigma[set, set]

        #Propose
        star <- coef
        star[set] <- rmvn(1, res_mean, res_Sigma)

        #Calculate log acceptance ratio
        diff_ll <- sum(ll_func(data[[outcome]], Xmat, star)) -
                   sum(ll_func(data[[outcome]], Xmat, coef))

        diff_prior <- dprior(star) - dprior(coef)

        diff_prop <- dmvn(star[set], res_mean, res_Sigma) -
                     dmvn(coef[set], res_mean, res_Sigma)

        log_AR <- diff_ll + diff_prior - diff_prop

        #Accept with correct probability
        if (log(runif(1)) < log_AR){
            coef <- star
            acc[set] <- 1
        }
    }

    #Return after all blocks update
    return(list(coef=coef, acc=acc))

}



#' Random walk Metropolis updating
#' Proposes from a MVN with mean centered at current value and
#' variance-covariance matrix prop_Sigma
#'
#' Supports split block updating
#'
#' @param coef Coefficient vector or matrix
#' @param outcome Character containing outcome variable name in data
#' @param data Data frame
#' @param Xmat Design matrix for regression
#' @param ll_func Function to calculate log-likelihood for model
#' @param dprior Function to evaluate prior density for coefficients
#' @param prop_Sigma Variance-covariance matrix for proposal distribution
#' @param block Vector or matrix of same dimension of coef, denoting with which
#' block the parameter should be updated. Defaults to update all parameters at
#' once (all block = 1).
#'
#' @return List containing updated coefficient (coef) and vector or matrix of
#' whether updates were accepted
#' @export
metrop <- function(coef, outcome,
                  data, Xmat,
                  ll_func, dprior,
                  prop_Sigma,
                  block = coef*0 + 1){

    #Keep track of acceptances for each parameter
    acc <- coef*0

    #Loop over the blocks
    for (i in 1:max(block)){
        set <- which(block == i)

        #Propose
        star <- coef
        star[set] <- rmvn(1, coef[set], prop_Sigma[set, set])

        #Calculate log acceptance ratio
        diff_ll <- sum(ll_func(data[[outcome]], Xmat, star)) -
                   sum(ll_func(data[[outcome]], Xmat, coef))

        diff_prior <- dprior(star) - dprior(coef)

        log_AR <- diff_ll + diff_prior

        #Accept with correct probability
        if (log(runif(1)) < log_AR){
            coef <- star
            acc[set] <- 1
        }
    }

    #Return after all blocks update
    return(list(coef=coef, acc=acc))

}
