dif_price_boots_basis<-function (param, basis_curveT2, proj_curveT2_dom, disc_curveT0_dom,
                                 proj_curveT2_for, disc_curveT0_for, base_dom, base_for, base_disc,
                                 days_freqT2_dom, days_freqT2_for, liq_dom, liq_for, freq_dom,
                                 freq_for, basis_factor, basis_float){
  n = length(param)
  basis_days = basis_curveT2[, 1]
  basis_rates = basis_curveT2[, 2]/basis_factor
  cc_days = basis_days[1]
  k = sum(disc_curveT0_dom[, 1] <= (cc_days - 15))
  days_freqT2_dom = days_freqT2_dom[days_freqT2_dom <= (tail(basis_days, 1) + 15)]
  posInf = findInterval(basis_days, days_freqT2_dom)
  close = ((days_freqT2_dom[posInf + 1] - basis_days) < (basis_days - days_freqT2_dom[posInf]))
  close[is.na(close)] = 0
  pos_dom = posInf + close
  pos_for = (pos_dom/freq_dom) * freq_for
  days_freqT2_for = days_freqT2_for[c(1:tail(pos_for, 1))]
  proj_freq_rates_dom = approx_extrap(x = proj_curveT2_dom[, 1], y = proj_curveT2_dom[, 2], xout = days_freqT2_dom)$y
  proj_freq_rates_for = approx_extrap(x = proj_curveT2_for[, 1], y = proj_curveT2_for[, 2], xout = days_freqT2_for)$y
  disc_freq_rates_dom = approx_extrap(x = c(disc_curveT0_dom[c(1:k),1], basis_days+liq_dom), y = c(disc_curveT0_dom[c(1:k), 2], param), xout = (days_freqT2_dom + liq_dom))$y
  disc_freq_rates_for = approx_extrap(x = disc_curveT0_for[, 1], y = disc_curveT0_for[, 2], xout = (days_freqT2_for + liq_for))$y

  lfreq_dom = length(days_freqT2_dom)
  lfreq_for = length(days_freqT2_for)
  delta_dom = (days_freqT2_dom - c(0, days_freqT2_dom[-lfreq_dom]))
  delta_for = (days_freqT2_for - c(0, days_freqT2_for[-lfreq_for]))
  proj_fwd_dom = ((1 + proj_freq_rates_dom * days_freqT2_dom/base_dom)/(1 + c(0, proj_freq_rates_dom[-lfreq_dom]) * c(0, days_freqT2_dom[-lfreq_dom])/base_dom) - 1) * base_dom/delta_dom
  proj_fwd_for = ((1 + proj_freq_rates_for * days_freqT2_for/base_for)/(1 + c(0, proj_freq_rates_for[-lfreq_for]) * c(0, days_freqT2_for[-lfreq_for])/base_for) -
                    1) * base_for/delta_for
  df_dom = 1/(1 + disc_freq_rates_dom * (days_freqT2_dom + liq_dom)/base_disc)
  df_for = 1/(1 + disc_freq_rates_for * (days_freqT2_for + liq_for)/base_disc)
  cum_proj_fwd_dom = cumsum(proj_fwd_dom * (delta_dom/base_dom) * df_dom) + df_dom

  cum_proj_fwd_for = cumsum(proj_fwd_for * (delta_for/base_for) * df_for) + df_for
  if (basis_float == 1) {
    cum_deltadf_dom = cumsum((delta_dom/base_dom) * df_dom)
    cum_basis=basis_rates * cum_deltadf_dom[pos_dom]
    ind_float = 1
  }
  if (basis_float == 0) {
    cum_deltadf_for = cumsum((delta_for/base_for) * df_for)
    cum_basis=basis_rates * cum_deltadf_for[pos_for]
    ind_float = -1
  }

  swap_pr =ind_float*cum_basis  + cum_proj_fwd_dom[pos_dom] -  cum_proj_fwd_for[pos_for]
  dif_sqrt = t(swap_pr) %*% swap_pr
  return(dif_sqrt)
}
