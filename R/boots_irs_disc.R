boots_irs_disc<-function (curveT2, base, cc_days, days_freq, liq_d, lb = 0, ub = 0.2) {
  k = sum(curveT2[, 1] <= cc_days)
  param = curveT2[-c(1:k), 2]
  l = length(param)
  lowerBounds = rep(lb, l)
  upperBounds = rep(ub, l)


  fit = nlminb(start = param, objective = dif_price_boots_irs,
               gradient = NULL, hessian = NULL, curveT2, base, cc_days,
               days_freq, liq_d, lower = lowerBounds, upper = upperBounds)

  cc_curve_T2=data.frame(V1=curveT2[,1], V2=c(curveT2[c(1:k),2],fit$par))
  cc_curve_T0=liq_t0(cc_curve_T2, liq_d, base)

  return(cc_curve_T0)
}
