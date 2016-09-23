#' Function to inflate/deflate proposal variance
#' 
#' Increases variance if accepting too often
#' Decreases variance if accepting too rarely
#' 
#' @param props List of proposal distributions
#' @param acc List of acceptances
#' @param blocks Blocking of parameter updating
#' @param iter Current (just-completed) iteration of the MCMC chains
#' @param lookback How many iterations to look back to calculate
#' the acceptance rate
#' @param tuneEvery How frequently to potentially modify proposals
#' @param stopAt Iteration at which point proposal is no longer
#' modified (should be at most the burn-in)
#' 
#' @return List of updated proposal distributions
#' 
tuneProposal <- function(props, acc, blocks,
                         iter, lookback, tuneEvery, stopAt) {
  
  # Keep everything same if not eligible to make changes
  if ((iter > stopAt) || (iter %% tuneEvery != 0) || (iter < lookback))
    return(props)
  
  # If eligible to make changes, ...
  # Loop over parameter models (Y, M, U)
  for (model in names(props)) {
    
    # Loop over blocks within model
    for (block in unique(blocks[[model]])) {
      
      # Number of parameters updated each iteration
      eachIter <- length(blocks[[model]])
      
      # Size of this particular block
      blockSize <- sum(blocks[[model]] == block)
      
      # Where to start looking in the indexing
      blockSet <- which(blocks[[model]] == block)
      first <- blockSet[1]
      
      # Indexing over entire acceptance array/matrix/vector
      startLook <- (iter - lookback) * eachIter + first
      endLook <- iter * eachIter
      relevant <- seq(startLook, endLook, by = eachIter)
      
      # Calculate acceptance of this block over relevant period
      accProp <- mean(acc[[model]][relevant])
      
      # Assign multiplier to inflate or deflate accordingly
      mult <- 1
      if (blockSize < 5) {
        if (accProp > 0.7) {
          mult <- 1.2
        } else if (accProp > 0.5) {
          mult <- 1.1
        } else if (accProp < 0.05) {
          mult <- 0.7
        } else if (accProp < 0.3) {
          mult <- 0.9
        }
      } else {
        if (accProp > 0.6) {
          mult <- 1.2
        } else if (accProp > 0.3) {
          mult <- 1.1
        } else if (accProp < 0.10) {
          mult <- 0.7
        } else if (accProp < 0.2) {
          mult <- 0.9
        }
      }
      
      # Apply changes to proposal covariance matrix
      V <- props[[model]][["Sigma"]][blockSet, blockSet]
      props[[model]][["Sigma"]][blockSet, blockSet] <- mult * V
      
    } # End looping over blocks
    
  } # End looping over models
  
  # Return new proposals
  return(props)
  
}
