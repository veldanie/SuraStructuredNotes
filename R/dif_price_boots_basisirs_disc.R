dif_price_boots_basisirs_disc=function (param, basis_curveT2, curveT2_fix, cc_days_fix, cc_days_float,
                                        base_fix, base_float, base_disc, days_freqT2_fix, days_freqT2_float,
                                        freq_fix, freq_float){
  k = sum(basis_curveT2[, 1] <= cc_days)
  n = length(param)
  basis_days = basis_curveT2[, 1]
  basis_rates = basis_curveT2[, 2]
  days_freqT2_fix = days_freqT2_fix[days_freqT2_fix <= (tail(basis_days,
                                                             1) + 15)]
  pos_fix = match(basis_days[-c(1:k)], days_freqT2_fix)
  pos_float = (pos_fix/freq_fix) * freq_float
  days_freqT2_float = days_freqT2_float[c(1:tail(pos_float,
                                                 1))]
  swap_rates_fix = approx_extrap(x = curveT2_fix[, 1], y = curveT2_fix[,2], xout = basis_days[-c(1:k)])$y

  disc_ratesT2=c(basis_rates[c(1:k)], param)
  freq_rates_float = approx_extrap(x = basis_days, y = disc_ratesT2, xout = days_freqT2_float)$y
  freq_rates_fix = approx_extrap(x = basis_days, y = disc_ratesT2, xout = days_freqT2_fix)$y


  lfreq_float = length(days_freqT2_float)
  lfreq_fix = length(days_freqT2_fix)
  delta_fix = (days_freqT2_fix - c(0, days_freqT2_fix[-length(days_freqT2_fix)]))
  delta_float = (days_freqT2_float - c(0, days_freqT2_float[-lfreq_float]))

  proj_fwd_float = ((1 + freq_rates_float * days_freqT2_float/base_float)/(1 +
                                                                             c(0, freq_rates_float[-lfreq_float]) * c(0, days_freqT2_float[-lfreq_float])/base_float) -
                      1) * base_float/delta_float


  df_fix = 1/(1 + freq_rates_fix * days_freqT2_fix/base_disc)
  df_float = 1/(1 + freq_rates_float * days_freqT2_float/base_disc)

  cum_df_fix = cumsum(df_fix*delta_fix/base_fix)
  cum_df_float = cumsum(df_float*delta_float/base_float)

  cum_proj_float = cumsum(proj_fwd_float * df_float * delta_float/base_float)

  swap_pr = swap_rates_fix * cum_df_fix[pos_fix] - (cum_proj_float[pos_float]+basis_rates[-c(1:k)]*cum_df_float[pos_float])
  dif_sqrt = t(swap_pr) %*% swap_pr
  return(dif_sqrt)
}
