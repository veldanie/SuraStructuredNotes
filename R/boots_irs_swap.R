boots_irs_swap<-function (basis_curveT2, proj_curveT2_float, disc_ratesT0, cc_days,
                          base_fix, base_float, base_disc, days_freqT2_fix, days_freqT2_float,
                          liq, freq_fix, freq_float, lb = 0, ub = 0.2){
  k = sum(basis_curveT2[, 1] <= cc_days)
  if(k>0){
    basis_days = basis_curveT2[-(1:k), 1]
    basis_rates = basis_curveT2[-(1:k), 2]
  }else{
    basis_days = basis_curveT2[, 1]
    basis_rates = basis_curveT2[, 2]
  }

  param = proj_curveT2_float[c(1:length(basis_rates)), 2]
  l = length(param)
  lowerBounds = rep(lb, l)
  upperBounds = rep(ub, l)
  fit = nlminb(start = param, objective = dif_price_boots_irs_swap,
               gradient = NULL, hessian = NULL, basis_curveT2, proj_curveT2_float,
               disc_ratesT0, cc_days, base_fix, base_float, base_disc,
               days_freqT2_fix, days_freqT2_float, liq, freq_fix, freq_float, lower = lowerBounds, upper = upperBounds)
  return(data.frame(V1 = basis_days, V2=fit$par))
}
