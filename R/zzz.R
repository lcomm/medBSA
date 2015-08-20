.onLoad <- function(libname, pkgname) {
    
    #Initialize environment for global variables
    assign("medBSA_env", new.env(), envir=parent.env(environment()))

}