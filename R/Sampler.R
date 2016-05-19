#Sampling script I will eventually function-ize for R package purposes
library("Rcpp")

source("./R/")
load("../BSA_MWE_data")

library("mvtnorm")


# Niceties ----------------------------------------------------------------

n <- length(Y)


# Options -----------------------------------------------------------------

intx <- TRUE
proptype <- "MLE"


# Functions ---------------------------------------------------------------



#Functions for evaluating prior on regression coefficients
dPriorY_coef <- function(x, prior = priors[["Y"]], log = FALSE){
    dmvnorm(x, mean = prior$mean, prior$Sigma, log = log)
}

dPriorM_coef <- function(x, prior = priors[["M"]], log = FALSE){
    dmvnorm(x, mean = prior$mean, prior$Sigma, log = log)
}

dPriorU_coef <- function(x, prior = priors[["U"]], log = FALSE){
    dmvnorm(x, mean = prior$mean, prior$Sigma, log = log)
}


#Functions for evaluating likelihoods (need dM, dY, dU)
#M should be able to be binary with logit link
#or categorical (with baselie category logit link)
#Y should be binary with logit link
#U should be binary with logit link (will extend this later)

family_M <- "multinomial"
family_M <- "binomial"

if (family_M == "multinomial"){
    dM <- function(y, p_mat, log = FALSE){
        dMultinom(probs = p_mat, y = y, lg = log)
    }

    propose_M_coef <- function(){

    }
} else if (family_M == "binomial"){
    dM <- function(y, p_vec, log = FALSE){
        dbinom(x = y, prob = p_vec, log = log)
    }
}


# Sampler -----------------------------------------------------------------


