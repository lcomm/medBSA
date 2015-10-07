#' Repeatedly try to come up with a valid result from a function, stopping if failing
#' 
#' This function allows for repeated tries (up to maxAttempts) of a function mainFun
#' to come up with result that is valid based on valFun.  Specifically, it is for 
#' times when an invalid results would be catastrophically bad.  It stops with an
#' error if it is unable to come up with a valid answer
#' 
#' @param mainFun The main generative function
#' @param valFun The function used to validate the answer from mainFun(); valid 
#' answers will return TRUE
#' @return A result from the mainFun() function
#' @seealso attemptCrit
#' @examples
#' ##Set the main function
#' mydraw <- function(...) { rnorm(1, ...) }
#' ##The validation function
#' myval <- function(rv) { (rv > 0) }
#' ##Try to draw a positive value from the normal distribution
#' ##Should work
#' attemptCrit(mydraw, myval, 1000, mean = 1, sd = 10)
#' ##Should fail
#' #attemptCrit(mydraw, myval, 10, mean = -100, sd = 0.01)
#' 
attemptCrit <- function(mainFun, valFun, maxAttempts, ...){
    for (attemptNum in 1:maxAttempts){
        #Do the function requested
        answer = mainFun(...)
        #Return if either valid or have reached max number of attempts
        if (valFun(answer) | (attemptNum == maxAttempts)) {
            if (valFun(answer) == FALSE) { 
                print(match.call)
                stop("ATTEMPTCRIT did not get valid answer in max number of attempts")
            }
            return(answer)
        }
    }
}
