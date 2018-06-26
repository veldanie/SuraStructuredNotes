dif_price_boots_proj=function(param, swap_curveT2, disc_ratesT0, cc_days, base_fix, base_float, base_disc, days_freqT2_fix, days_freqT2_float, liq, freq_fix, freq_float){

  n = length(param)
  swap_days = swap_curveT2[,1]
  swap_rates = swap_curveT2[,2]
  days_freqT2_fix = days_freqT2_fix[days_freqT2_fix <= (tail(swap_days,1)+30)]#Se suma 30 para garantizar que no se elimine un tenor requerido

  k = sum(swap_curveT2$V1 <= cc_days)
  pos_fix = match(swap_days[-c(1:k)], days_freqT2_fix)
  pos_float = (pos_fix/freq_fix)*freq_float

  days_freqT2_float = days_freqT2_float[c(1:tail(pos_float,1))]

  proj_freq_rates = approx_extrap(x=c(swap_days[c(1:k)],days_freqT2_float[pos_float]),y=c(swap_rates[c(1:k)], param),xout=days_freqT2_float)$y
  lfreq_float=length(days_freqT2_float)
  lfreq_fix=length(days_freqT2_fix)

  delta_float = (days_freqT2_float - c(0, days_freqT2_float[-lfreq_float]))
  delta_fix = (days_freqT2_fix - c(0, days_freqT2_fix[-lfreq_fix]))

  proj_fwd=((1+proj_freq_rates*days_freqT2_float/base_float)/(1+c(0,proj_freq_rates[-lfreq_float])*c(0,days_freqT2_float[-lfreq_float])/base_float)-1)*base_float/delta_float

  df_float = 1/(1 + disc_ratesT0[(days_freqT2_float+liq)] * (days_freqT2_float+liq)/base_disc)#Discont Factors to Valuation Date
  cum_proj_fwd=cumsum(proj_fwd*(delta_float/base_float)*df_float)

  #Para DF de la pata fija, se usan los d?as de la pata variable, los cuales se asumen siempre REALES.
  days_freqT2_fix_disc=days_freqT2_float[(freq_float*c(1:lfreq_fix)/freq_fix)]+liq
  df_fix = 1/(1 + disc_ratesT0[days_freqT2_fix_disc] * (days_freqT2_fix_disc)/base_disc)#Discont Factors to Valuation Date
  cum_deltaDF = cumsum((delta_fix/base_fix) * df_fix)

  swap_pr=swap_rates[-c(1:k)]*cum_deltaDF[pos_fix] - cum_proj_fwd[pos_float]

  dif_sqrt = t(swap_pr) %*% swap_pr
  return(dif_sqrt)
}
