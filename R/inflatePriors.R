#' Inflated variance of prior distributions by appriorriate prior taus
#' 
#' If a prior tau exists in the environment (tauPriorY, tauPriorU, or tauPriorM,
#' if doing the mediator case), then this adjusts the prior by the appriorriate
#' factor
#' 
#' @seealso inflateProps
#' 
inflatePriors <- function(){
    
    #Inflate Y prior variance if necessary
    if (exists("tauPriorY") && (tauPriorY != 1)) {
        priors[["Y"]][["Sigma"]] <<- priors[["Y"]][["Sigma"]] * tauPriorY
    }
    
    #Inflate U prior variance if necessary
    if (exists("tauPriorU") && (tauPriorU != 1)) {
        priors[["U"]][["Sigma"]] <<- priors[["U"]][["Sigma"]] * tauPriorU
    }
    
    #Do M as well if mediator case
    if (case == "mediation" && exists("tauPriorM")){
        priors[["M"]][["Sigma"]] <<- priors[["M"]][["Sigma"]] * tauPriorM
    }
    
}

