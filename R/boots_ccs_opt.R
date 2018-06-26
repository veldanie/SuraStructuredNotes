boots_ccs_opt=function (ccs_curveT2, disc_curveT0_dom, proj_curveT2_for,
                        disc_curveT0_for, base_dom, base_for, base_disc, days_freqT2_dom,
                        days_freqT2_for, liq_dom, liq_for, freq_dom, freq_for, lb = -0.2,
                        ub = 0.2){
  ccs_days = ccs_curveT2[, 1]
  posInf = findInterval(ccs_days, disc_curveT0_dom[, 1])
  close = ((disc_curveT0_dom[posInf + 1, 1] - ccs_days) < (ccs_days -
                                                             disc_curveT0_dom[posInf, 1]))
  close[is.na(close)] = 0
  posDisc = posInf + close
  param = disc_curveT0_dom[posDisc, 2]
  l = length(param)
  lowerBounds = rep(lb, l)
  upperBounds = rep(ub, l)
  fit = nlminb(start = param, objective = dif_price_boots_ccs,
               gradient = NULL, hessian = NULL, ccs_curveT2,
               disc_curveT0_dom, proj_curveT2_for, disc_curveT0_for,
               base_dom, base_for, base_disc, days_freqT2_dom, days_freqT2_for,
               liq_dom, liq_for, freq_dom, freq_for, lower = lowerBounds,
               upper = upperBounds)
  cc_days = ccs_curveT2[1, 1]
  k = sum(disc_curveT0_dom[, 1] <= (cc_days - 15))
  cc_curve = data.frame(V1 = c(disc_curveT0_dom[c(1:k), 1],
                               (ccs_curveT2[, 1] + liq_dom)), V2 = c(disc_curveT0_dom[c(1:k),
                                                                                      2], fit$par))
  return(cc_curve)
}
