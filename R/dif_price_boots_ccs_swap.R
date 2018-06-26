dif_price_boots_ccs_swap<-function (param, basis_curveT2_for, disc_curveT0_dom, proj_curveT2_for,
                                    disc_curveT0_for, base_dom, base_for, base_disc, days_freqT2_dom,
                                    days_freqT2_for, liq_dom, liq_for, freq_dom, freq_for){
  pos_swap=match(basis_curveT2_for[, 1],days_freqT2_dom)
  ccs_days = basis_curveT2_for[!is.na(pos_swap), 1]
  days_freqT2_dom = days_freqT2_dom[days_freqT2_dom <= (tail(ccs_days, 1) + 15)]
  posInf = findInterval(ccs_days, days_freqT2_dom)
  close = ((days_freqT2_dom[posInf + 1] - ccs_days) < (ccs_days - days_freqT2_dom[posInf]))
  close[is.na(close)] = 0
  pos_dom = posInf + close
  pos_for = (pos_dom/freq_dom) * freq_for
  days_freqT2_for = days_freqT2_for[c(1:tail(pos_for, 1))]
  proj_freq_rates_for = approx_extrap(x = proj_curveT2_for[, 1], y = proj_curveT2_for[, 2], xout = days_freqT2_for)$y
  posInf = findInterval(ccs_days, disc_curveT0_dom[, 1])
  close = ((disc_curveT0_dom[posInf + 1, 1] - ccs_days) < (ccs_days - disc_curveT0_dom[posInf, 1]))
  close[is.na(close)] = 0
  posDisc = posInf + close
  disc_freq_rates_dom = approx_extrap(x = disc_curveT0_dom[, 1], y = disc_curveT0_dom[, 2], xout = (days_freqT2_dom + liq_for))$y
  disc_freq_rates_for = approx_extrap(x = disc_curveT0_for[, 1], y = disc_curveT0_for[, 2], xout = (days_freqT2_for + liq_for))$y
  lfreq_dom = length(days_freqT2_dom)
  lfreq_for = length(days_freqT2_for)
  delta_dom = (days_freqT2_dom - c(0, days_freqT2_dom[-lfreq_dom]))
  delta_for = (days_freqT2_for - c(0, days_freqT2_for[-lfreq_for]))
  proj_fwd_for = ((1 + proj_freq_rates_for * days_freqT2_for/base_for)/(1 + c(0, proj_freq_rates_for[-lfreq_for]) * c(0, days_freqT2_for[-lfreq_for])/base_for) - 1) * base_for/delta_for
  df_dom = 1/(1 + disc_freq_rates_dom * (days_freqT2_dom + liq_dom)/base_disc)
  df_for = 1/(1 + disc_freq_rates_for * (days_freqT2_for + liq_for)/base_disc)
  cum_deltadf_dom = cumsum((delta_dom/base_dom) * df_dom)
  cum_proj_fwd_for = cumsum(proj_fwd_for * (delta_for/base_for) * df_for) + df_for
  swap_pr = param * cum_deltadf_dom[pos_dom] + df_dom[pos_dom] - cum_proj_fwd_for[pos_for]
  dif_sqrt = t(swap_pr) %*% swap_pr
  return(dif_sqrt)
}
