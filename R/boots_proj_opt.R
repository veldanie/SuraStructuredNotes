boots_proj_opt=function (swap_curveT2, disc_ratesT0, cc_days, base_fix, base_float, base_disc, days_freqT2_fix, days_freqT2_float, liq, freq_fix, freq_float, lb=0, ub=0.2){
  k = sum(swap_curveT2[,1] <=cc_days)
  param=swap_curveT2[-c(1:k),2]
  l = length(param)
  lowerBounds = rep(lb, l)
  upperBounds = rep(ub, l)
  fit = nlminb(start = param, objective = dif_price_boots_proj, gradient = NULL, hessian = NULL,
               swap_curveT2, disc_ratesT0, cc_days, base_fix, base_float, base_disc, days_freqT2_fix, days_freqT2_float, liq, freq_fix, freq_float,
               lower = lowerBounds, upper = upperBounds)

  pos_fix = match(swap_curveT2[-c(1:k),1], days_freqT2_fix)
  pos_float = (pos_fix/freq_fix)*freq_float

  return(data.frame(V1 = c(swap_curveT2[c(1:k),1],days_freqT2_float[pos_float]), V2 = c(swap_curveT2[c(1:k),2],fit$par)))
}
