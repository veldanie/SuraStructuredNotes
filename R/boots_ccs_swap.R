boots_ccs_swap<-function (disc_curveT0_dom, basis_curveT2_for, proj_curveT2_for, disc_curveT0_for,
                          base_dom, base_for, base_disc, days_freqT2_dom, days_freqT2_for,
                          liq_dom, liq_for, freq_dom, freq_for, lb = -0.2, ub = 0.2){
  #Se genera tasa swap en los nodos donde hay flujo de caja.
  pos_swap=match(basis_curveT2_for[, 1],days_freqT2_dom)
  ccs_days = basis_curveT2_for[!is.na(pos_swap), 1]
  posInf = findInterval(ccs_days, disc_curveT0_dom[, 1])
  close = ((disc_curveT0_dom[posInf + 1, 1] - ccs_days) < (ccs_days - disc_curveT0_dom[posInf, 1]))
  close[is.na(close)] = 0
  posDisc = posInf + close
  param = disc_curveT0_dom[posDisc, 2]
  l = length(param)
  lowerBounds = rep(lb, l)
  upperBounds = rep(ub, l)
  fit = nlminb(start = param, objective = dif_price_boots_ccs_swap,
               gradient = NULL, hessian = NULL, basis_curveT2_for, disc_curveT0_dom,
               proj_curveT2_for, disc_curveT0_for, base_dom, base_for,
               base_disc, days_freqT2_dom, days_freqT2_for, liq_dom,
               liq_for, freq_dom, freq_for, lower = lowerBounds, upper = upperBounds)
  cc_curve = data.frame(V1 = ccs_days, V2 = fit$par)
  return(cc_curve)
}
