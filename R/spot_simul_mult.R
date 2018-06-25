
spot_simul_mult <-function(spot, times, rates, sigma, rho, M = 1000){

    nact = length(spot)
    t_mat = times %*% t(rep(1, nact))
    SIGMA = diag(sigma) %*% rho %*% diag(sigma)
    nu = rates- sigma^2/2
    N = length(times)
    R = chol(SIGMA)
    st = array(0, dim = c(N, M, nact))
    
    for (i in 1:M) {
      x = matrix(rnorm(N * nact), ncol = nact) * sqrt(diff(c(0, times)))
      w = x %*% R
      wt = apply(w, 2, cumsum)
      st[, i, ] = exp(repr(nu, N) * t_mat + wt) %*% diag(spot)
    }
    return(st)
}
