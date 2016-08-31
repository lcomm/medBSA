sampler <- function(data,
                    Mformula,
                    Yformula,
                    Uformula,
                    priors,
                    props,
                    state,
                    niter = 100){

    ### Pre-loop prep
    #Update from existing state (or initial values)
    coef_Y <- state$coef_Y
    coef_M <- state$coef_M
    coef_U <- state$coef_U
    set.seed(state$seed)

    # Make constant design matrices
    XmatU <- make_Xmat(Uformula, data = data)
    XmatM <- make_Xmat(Mformula, data = data)
    XmatY <- make_Xmat(Yformula, data = data)
    XmatY_U1 <- make_Xmat_set(Yformula, data = data, "U", 1)
    XmatY_U0 <- make_Xmat_set(Yformula, data = data, "U", 0)
    XmatM_U1 <- make_Xmat_set(Mformula, data = data, "U", 1)
    XmatM_U0 <- make_Xmat_set(Mformula, data = data, "U", 0)

    #TODO: finish rest of sampler
}
