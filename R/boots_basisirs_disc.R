boots_basisirs_disc=function (basis_curveT2, curveT2_fix, cc_days_fix, cc_days_float,
                              base_fix, base_float, base_disc, days_freqT2_fix, days_freqT2_float,
                              freq_fix, freq_float, lb = 0, ub = 0.2){
  if(cc_days_float<cc_days_fix){stop("El plazo de la m?xima tasas CC de OIS debe ser mayor a la max tasa CC en la curva Swap Libor 3M")}
  k = sum(basis_curveT2[, 1] <= cc_days)
  param = basis_curveT2[-c(1:k), 2]
  l = length(param)
  lowerBounds = rep(lb, l)
  upperBounds = rep(ub, l)
  fit = nlminb(start = param, objective = dif_price_boots_basisirs_disc,
               gradient = NULL, hessian = NULL, basis_curveT2, curveT2_fix, cc_days_fix, cc_days_float,
               base_fix, base_float, base_disc, days_freqT2_fix, days_freqT2_float,
               freq_fix, freq_float, lower = lowerBounds, upper = upperBounds)
  pos_fix = match(basis_curveT2[-c(1:k), 1], days_freqT2_fix)
  pos_float = (pos_fix/freq_fix) * freq_float
  return(data.frame(V1 = c(basis_curveT2[c(1:k), 1], days_freqT2_float[pos_float]),
                    V2 = c(basis_curveT2[c(1:k), 2], fit$par)))
}
