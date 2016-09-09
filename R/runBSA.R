#' Function to run the sensitivity analysis
runBSA <- function(data,
                   formulas,
                   priors,
                   props,
                   state = NULL,
                   blocks = NULL,
                   niter = 100){

  ### Pre-loop prep
  Yformula <- formulas$Y
  Mformula <- formulas$M
  Uformula <- formulas$U
  
  # Update from existing state (or initial values) if provided
  coef_Y <- priors[["Y"]][["mean"]]
  coef_M <- matrix(priors[["M"]][["mean"]], 
                   ncol = length(unique(data$M)) - 1)
  coef_U <- priors[["U"]][["mean"]]

  # Initialize result container
  result        <- list()
  result$coef_Y <- addDimension(coef_Y, niter, NA)
  result$coef_M <- addDimension(coef_M, niter, NA)
  result$coef_U <- addDimension(coef_U, niter, NA)
  result$acc    <- list(Y = addDimension(coef_Y, niter, NA),
                        M = addDimension(coef_M, niter, NA),
                        U = addDimension(coef_U, niter, NA))
  
  # Set log-prior functions
  dpriorY <- function(x)
    dmvn(x, priors[["Y"]][["mean"]], priors[["Y"]][["Sigma"]], log = TRUE)
  
  dpriorM <- function(x)
    dmvn(c(x), priors[["M"]][["mean"]], priors[["M"]][["Sigma"]], log = TRUE)
  
  dpriorU <- function(x)
    dmvn(x, priors[["U"]][["mean"]], priors[["U"]][["Sigma"]], log = TRUE)
  
  # Set proposal functions
  if (is.null(props[["Y"]][["type"]]))
    props[["Y"]][["type"]] <- "random walk"
  if (is.null(props[["M"]][["type"]]))
    props[["M"]][["type"]] <- "random walk"
  if (is.null(props[["U"]][["type"]]))
    props[["U"]][["type"]] <- "random walk"
  
  sample_coef_Y <- switch(props[["Y"]][["type"]],
                          "independence" = indep,
                          "random walk" = metrop)
  sample_coef_M <- switch(props[["M"]][["type"]],
                          "independence" = indep,
                          "random walk" = metrop)
  sample_coef_U <- switch(props[["U"]][["type"]],
                          "independence" = indep,
                          "random walk" = metrop)

  
  # Use default blocking if none were provided
  if (is.null(blocks))
    blocks <- list(Y = makeBlocks(coef_Y, "together"), 
                   M = makeBlocks(coef_M, "cols"), 
                   U = makeBlocks(coef_U, "together"))

  
  # Make constant design matrices
  XmatU    <- make_Xmat(Uformula, data = data)
  XmatY_U1 <- make_Xmat_set(Yformula, data = data, "U", 1)
  XmatY_U0 <- make_Xmat_set(Yformula, data = data, "U", 0)
  XmatM_U1 <- make_Xmat_set(Mformula, data = data, "U", 1)
  XmatM_U0 <- make_Xmat_set(Mformula, data = data, "U", 0)

  
  ### Loop
  for (iter in 1:niter){
    ## I-step: imputation of U
    pU     <- medBSA:::get_pU1(XmatM_U0, XmatM_U1, coef_M, data$M,
                               XmatY_U0, XmatY_U1, coef_Y, data$Y,
                               XmatU, coef_U)
    data$U <- medBSA::rbern(pU)
    
    #Update design matrices that need to change with new imputed U
    XmatM <- make_Xmat(Mformula, data)
    XmatY <- make_Xmat(Yformula, data)
    
    
    ## P-step: updating of parameters
    # Y model coefficients
    resY <- sample_coef_Y(coef_Y, "Y", data, XmatY, ll_Y, dpriorY, 
                          props[["Y"]][["mean"]], 
                          props[["Y"]][["Sigma"]],
                          blocks$Y)
    coef_Y <- resY$coef
    
    # M model coefficients
    resM <- sample_coef_M(coef_M, "M", data, XmatM, ll_M, dpriorM, 
                          props[["M"]][["mean"]], 
                          props[["M"]][["Sigma"]],
                          blocks$M)
    coef_M <- resM$coef
    
    # U model coefficients
    resU <- sample_coef_U(coef_U, "U", data, XmatU, ll_U, dpriorU, 
                          props[["U"]][["mean"]], 
                          props[["U"]][["Sigma"]],
                          blocks$U)
    coef_U <- resU$coef
    
    # Save coefficient results of this iteration
    result$coef_Y[, iter]   <- coef_Y
    result$coef_M[, , iter] <- coef_M
    result$coef_U[, iter]   <- coef_U
    
    # Save acceptance results of this iteration
    result$acc[["Y"]][, iter]   <- resY$acc
    result$acc[["M"]][, , iter] <- resM$acc
    result$acc[["U"]][, iter]   <- resU$acc
    
    # Tune proposal distribution
    props <- tuneProposal(props, result$acc, blocks, iter, 
                          lookback = 100, tuneEvery = 200, stopAt = 5000)
    
  } # End of chain
  
  
  ### Post-loop
  result$state <- list(coef_Y = coef_Y, 
                       coef_M = coef_M, 
                       coef_U = coef_U, 
                       seed=.Random.seed)
  class(result) <- "medBSA"
  return(result)
  
}



