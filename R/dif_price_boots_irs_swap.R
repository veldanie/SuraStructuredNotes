dif_price_boots_irs_swap<-function (param, basis_curveT2, proj_curveT2_float, disc_ratesT0,
                                    cc_days, base_fix, base_float, base_disc, days_freqT2_fix,
                                    days_freqT2_float, liq, freq_fix, freq_float){
  k = sum(basis_curveT2[, 1] <= cc_days)
  if(k>0){
    basis_days = basis_curveT2[-(1:k), 1]
    basis_rates = basis_curveT2[-(1:k), 2]
  }else{
    basis_days = basis_curveT2[, 1]
    basis_rates = basis_curveT2[, 2]
  }
  days_freqT2_fix = days_freqT2_fix[days_freqT2_fix <= (tail(basis_days, 1) + 15)]
  pos_fix = match(basis_days, days_freqT2_fix)
  pos_float = (pos_fix/freq_fix) * freq_float
  days_freqT2_float = days_freqT2_float[c(1:tail(pos_float, 1))]
  proj_freq_rates_float = approx_extrap(x = proj_curveT2_float[,1], y = proj_curveT2_float[, 2], xout = days_freqT2_float)$y

  lfreq_fix = length(days_freqT2_fix)
  lfreq_float = length(days_freqT2_float)
  delta_float = (days_freqT2_float - c(0, days_freqT2_float[-length(days_freqT2_float)]))
  delta_fix = (days_freqT2_fix - c(0, days_freqT2_fix[-lfreq_fix]))
  proj_fwd_float = ((1 + proj_freq_rates_float * days_freqT2_float/base_float)/(1 + c(0, proj_freq_rates_float[-lfreq_float]) * c(0, days_freqT2_float[-lfreq_float])/base_float) - 1) * base_float/delta_float
  df_float = 1/(1 + disc_ratesT0[(days_freqT2_float + liq)] * (days_freqT2_float + liq)/base_disc)
  cum_proj_float = cumsum(proj_fwd_float * (delta_float/base_float) * df_float)
  cum_basisDF = cumsum((delta_float/base_float) * df_float)
  df_fix = 1/(1 + disc_ratesT0[(days_freqT2_fix + liq)] * (days_freqT2_fix + liq)/base_disc)
  cum_basisDF_float = cumsum((delta_float/base_float) * df_float)
  cum_basisDF_fix = cumsum((delta_fix/base_fix) * df_fix)
  swap_pr = basis_rates * cum_basisDF_float[pos_float] + cum_proj_float[pos_float] -
    param * cum_basisDF_fix[pos_fix]
  dif_sqrt = t(swap_pr) %*% swap_pr
  return(dif_sqrt)
}
