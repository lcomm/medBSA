#' Function to make prior list based on MLE data fits
#' 
#' (For now, assumes logit link and categorical M)
#' 
#' @param data Data frame for fitting models
#' @param formulas List of formulas for regression models
#' 
#' @return List of prior means and variances
#' @export
makePriors <- function(data, formulas){
  
  # Need VGAM for baseline category logit models
  require("VGAM", quietly = TRUE)
  
  # Fit regression models to small data set
  fitY <- glm(formulas$Yformula, family = binomial(link="logit"), data)
  fitM <- vglm(formulas$Mformula, family = multinomial(refLevel=1), data)
  fitU <- glm(formulas$Uformula, family = binomial(link="logit"), data)
  
  # Prepare to reorder coefficient matrix for M
  newOrderM <- c(matrix(1:length(coef(fitM)), 
                        ncol = length(unique(small$M)) - 1, 
                        byrow=TRUE))
  
  # Turn into priors
  priors <- list(Y = list(mean = unname(coef(fitY)),
                          Sigma = unname(vcov(fitY))),
                 M = list(mean = unname(coef(fitM))[newOrderM],
                          Sigma = unname(vcov(fitM))[newOrderM, newOrderM]),
                 U = list(mean = unname(coef(fitU)),
                          Sigma = unname(vcov(fitU))))
  
  # All of U regression needs to be informative 
  notUcoef <- list(Y = grep("U", names(coef(fitY)), invert = TRUE),
                   M = grep("U", names(coef(fitM))[newOrderM]), invert = TRUE)
    
  # Make everything else non-informative
  priors$Y[["Sigma"]] <- inflateVariance(priors$Y[["Sigma"]], 1000, notUcoef$Y)
  priors$M[["Sigma"]] <- inflateVariance(priors$M[["Sigma"]], 1000, notUcoef$M)

  # Return
  return(priors)
  
}
