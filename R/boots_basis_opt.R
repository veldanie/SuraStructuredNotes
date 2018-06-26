boots_basis_opt<-function (basis_curveT2, proj_curveT2_dom, disc_curveT0_dom,
                           proj_curveT2_for, disc_curveT0_for, base_dom, base_for, base_disc,
                           days_freqT2_dom, days_freqT2_for, liq_dom, liq_for, freq_dom,
                           freq_for, basis_factor = 10000, basis_float = 1, lb = -0.2, ub = 0.2)
{
  basis_days = basis_curveT2[, 1]
  posInf = findInterval(basis_days, disc_curveT0_dom[, 1])
  close = ((disc_curveT0_dom[posInf + 1, 1] - basis_days) < (basis_days - disc_curveT0_dom[posInf, 1]))
  close[is.na(close)] = 0
  posDisc = posInf + close
  param = disc_curveT0_dom[posDisc, 2]
  l = length(param)
  lowerBounds = rep(lb, l)
  upperBounds = rep(ub, l)
  fit = nlminb(start = param, objective = dif_price_boots_basis,
               gradient = NULL, hessian = NULL, basis_curveT2, proj_curveT2_dom,
               disc_curveT0_dom, proj_curveT2_for, disc_curveT0_for,
               base_dom, base_for, base_disc, days_freqT2_dom, days_freqT2_for,
               liq_dom, liq_for, freq_dom, freq_for, basis_factor, basis_float, lower = lowerBounds,
               upper = upperBounds)
  cc_days = basis_curveT2[1, 1]
  k = sum(disc_curveT0_dom[, 1] <= (cc_days - 15))
  cc_curve = data.frame(V1 = c(disc_curveT0_dom[c(1:k), 1],
                               (basis_curveT2[, 1] + liq_dom)), V2 = c(disc_curveT0_dom[c(1:k),
                                                                                        2], fit$par))
  return(cc_curve)
}
