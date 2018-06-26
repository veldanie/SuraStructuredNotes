boots_basisirs_proj=function (basis_curveT2, proj_curveT2_fix, disc_ratesT0, cc_days, base_fix, base_float, base_disc, days_freqT2_fix, days_freqT2_float, liq, freq_fix, freq_float, basis_float=1, lb=0, ub=0.2){
  #El t?rmino "fix" hace referencia a la curva de proyecci?n conocida.
  #Si basis_fix=1; basis se suma sobre la curva de proyecci?n conocida. Si basis_fix=0; basis se suma sobre la curva de proyecci?n buscada.
  k = sum(basis_curveT2[,1] <=cc_days)
  param=proj_curveT2_fix[c(1:(length(basis_curveT2[,2])-k)),2]
  l = length(param)
  lowerBounds = rep(lb, l)
  upperBounds = rep(ub, l)
  fit = nlminb(start = param, objective = dif_price_boots_basisirs, gradient = NULL, hessian = NULL,
               basis_curveT2, proj_curveT2_fix, disc_ratesT0, cc_days, base_fix, base_float, base_disc, days_freqT2_fix, days_freqT2_float, liq, freq_fix, freq_float, basis_float,
               lower = lowerBounds, upper = upperBounds)

  pos_fix = match(basis_curveT2[-c(1:k),1], days_freqT2_fix)
  pos_float = (pos_fix/freq_fix)*freq_float

  return(data.frame(V1 = c(basis_curveT2[c(1:k),1],days_freqT2_float[pos_float]), V2 = c(basis_curveT2[c(1:k),2],fit$par)))
}
