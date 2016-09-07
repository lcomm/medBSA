#' Function to run the sensitivity analysis
runBSA <- function(data,
                    formulas,
                    priors,
                    props,
                    state,
                    blocks = NULL,
                    niter = 100){

  ### Pre-loop prep
  Yformula <- formulas$Y
  Mformula <- formulas$M
  Uformula <- formulas$U
  
  # Update from existing state (or initial values)
  coef_Y <- state$coef_Y
  coef_M <- state$coef_M
  coef_U <- state$coef_U
  set.seed(state$seed)

  # Initialize result container
  result <- list()
  result$coef_Y <- addDimension(coef_Y, niter, NA)
  result$coef_M <- addDimension(coef_M, niter, NA)
  result$coef_U <- addDimension(coef_U, niter, NA)
  
  # Set log-prior functions
  dprior_Y <- function(x)
    dmvn(x, priors[["Y"]][["mean"]], priors[["Y"]][["Sigma"]], log = TRUE)
  
  dprior_M <- function(x)
    dmvn(c(x), priors[["M"]][["mean"]], priors[["M"]][["Sigma"]], log = TRUE)
  
  dprior_U <- function(x)
    dmvn(x, priors[["U"]][["mean"]], priors[["U"]][["Sigma"]], log = TRUE)
  
  
  # Update blocking
  if (is.null(blocks))
    blocks <- list(Y = coef_Y*0 + 1, M = coef_M*0 + 1, U = coef_U*0 + 1)

  
  # Make constant design matrices
  XmatU    <- make_Xmat(Uformula, data = data)
  XmatY_U1 <- make_Xmat_set(Yformula, data = data, "U", 1)
  XmatY_U0 <- make_Xmat_set(Yformula, data = data, "U", 0)
  XmatM_U1 <- make_Xmat_set(Mformula, data = data, "U", 1)
  XmatM_U0 <- make_Xmat_set(Mformula, data = data, "U", 0)

  #TODO: finish rest of sampler
  ### Loop
  for (iter in 1:niter){
    ## I-step: imputation of U
    data$U <- medBSA::rbern(prob = medBSA:::get_pU1(XmatM_U0,
                                                    XmatM_U1,
                                                    coef_M,
                                                    data$M,
                                                    XmatY_U0,
                                                    XmatY_U1,
                                                    coef_Y,
                                                    data$Y,
                                                    XmatU,
                                                    coef_U))
    
    #Update design matrices that need to change with new U
    XmatM <- make_Xmat(Mformula, data)
    XmatY <- make_Xmat(Yformula, data)
    
    
    ## P-step: updating of parameters
    # Y model coefficients
    resY <- indep(coef_Y, "Y", data, XmatY, ll_Y, dpriorY, 
                  props[["Y"]][["mean"]], props[["Y"]][["Sigma"]])
    coef_Y <- resY$coef
    acc$Y <- acc$Y + resY$acc
    
    # M model coefficients
    resM <- indep(coef_M, "M", data, XmatM, ll_M, dpriorM, 
                  props[["M"]][["mean"]], props[["M"]][["Sigma"]])
    coef_M <- resM$coef
    acc$M <- acc$M + resM$acc
    
    # U model coefficients
    resU <- indep(coef_U, "U", data, XmatU, ll_U, dpriorU, 
                  props[["U"]][["mean"]], props[["U"]][["Sigma"]])
    coef_U <- resU$coef
    acc$U <- acc$U + resU$acc
    
    # Save results
    result$coef_Y[, iter] <- coef_Y
    result$coef_M[, , iter] <- coef_M
    result$coef_U[, iter] <- coef_U
  }
  
  
  ### Post-loop
  result$state <- list(coef_Y = coef_Y, 
                       coef_M = coef_M, 
                       coef_U = coef_U, 
                       seed=.Random.seed)
  class(result) <- "medBSA"
  return(result)
}



