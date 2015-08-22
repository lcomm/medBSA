#' Inflated variance of proposal distributions by appropriate proposal taus
#' 
#' If a proposal tau exists in the environment (tauPropY, tauPropU, or tauPropM,
#' if doing the mediator case), then this adjusts the proposal by the appropriate
#' factor
#' 
inflateProps <- function(){
    
    #Inflate Y proposal variance if necessary
    if (exists("tauPropY") && (tauPropY != 1)) {
        props[["Y"]][["Sigma"]] <<- props[["Y"]][["Sigma"]] * tauPropY
    }
    
    #Inflate U proposal variance if necessary
    if (exists("tauPropU") && (tauPropU != 1)) {
        props[["U"]][["Sigma"]] <<- props[["U"]][["Sigma"]] * tauPropU
    }
    
    #Do M as well if mediator case
    if (case == "mediator" && exists("tauPropM")){
        props[["M"]][["Sigma"]] <<- props[["M"]][["Sigma"]] * tauPropM
    }
    
}

