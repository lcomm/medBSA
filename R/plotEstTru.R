#' Plot estimated parameters against true parameters
#' 
#' This function takes a vector of parameter estimates, a vector of true values,
#' and a parameter name and it makes an attractive plot to demonstrate unbiasedness
#' and identifiability (or lack thereof)
#' 
#' @param est The vector of parameter estimates
#' @param tru The vector of true parameter values corresponding to the estimates
#' @param paramname The label (used only in the title of the plot)
#' @examples
#' ##Generate some values
#' tru = 1:100
#' est = rnorm(100, mean = tru, sd = 5)
#' ##Plot them against each other
#' plotEstTru(est, tru, paramname = "My Favorite Parameter")
#' 
plotEstTru <- function(est, tru, paramname){
    #For labels
    est.mean = round(mean(est, na.rm = TRUE), 3)
    tru.mean = round(mean(tru, na.rm = TRUE), 3)
    
    #Identical X and Y axes
    lims = c(min(est, tru, na.rm = TRUE),
             max(est, tru, na.rm = TRUE))
    
    #Plot
    plot(est ~ tru,
         ylim = lims,
         xlim = lims,
         main = paramname, 
         ylab = "Estimated",
         xlab = "True",
         sub = paste0("Mean Est: ",est.mean,", Mean True: ",tru.mean),
         lwd=2)
    abline(0, 1, lty=2)
    
}