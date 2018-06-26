basis_imp_opt=function(disc_curveT0_dom, proj_curveT2_dom, base, cc_days, days_freq, liq_dom, lb = -0.2, ub = 0.2){

  k = sum(disc_curveT0_dom[, 1] <= cc_days)
  param = disc_curveT0_dom[-c(1:k), 2]
  l = length(param)
  lowerBounds = rep(lb, l)
  upperBounds = rep(ub, l)
  fit = nlminb(start = param, objective = dif_price_basis_imp,
               gradient = NULL, hessian = NULL, disc_curveT0_dom, proj_curveT2_dom, base, cc_days, days_freq, liq_dom, lower = lowerBounds, upper = upperBounds)
  return(data.frame(V1 = disc_curveT0_dom[-1, 1]-liq_dom, V2 = fit$par))
}
