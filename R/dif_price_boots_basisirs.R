dif_price_boots_basisirs=function(param, basis_curveT2, proj_curveT2_fix, disc_ratesT0, cc_days, base_fix, base_float, base_disc, days_freqT2_fix, days_freqT2_float, liq, freq_fix, freq_float, basis_float=1){

  k = sum(basis_curveT2[,1] <=cc_days)
  n = length(param)

  basis_days=basis_curveT2[,1]
  basis_rates=basis_curveT2[,2]

  days_freqT2_fix = days_freqT2_fix[days_freqT2_fix <= (tail(basis_days,1)+15)]

  pos_fix = match(basis_days[-c(1:k)], days_freqT2_fix)
  pos_float = (pos_fix/freq_fix)*freq_float

  days_freqT2_float = days_freqT2_float[c(1:tail(pos_float,1))]


  proj_freq_rates_fix = approx_extrap(x=proj_curveT2_fix[,1], y=proj_curveT2_fix[,2],xout=days_freqT2_fix)$y
  proj_freq_rates_float = approx_extrap(x=c(basis_days[c(1:k)],days_freqT2_float[pos_float]),y=c(basis_rates[c(1:k)], param),xout=days_freqT2_float)$y

  lfreq_float=length(days_freqT2_float)
  lfreq_fix=length(days_freqT2_fix)

  delta_fix = (days_freqT2_fix - c(0, days_freqT2_fix[-length(days_freqT2_fix)]))
  delta_float = (days_freqT2_float - c(0, days_freqT2_float[-lfreq_float]))

  proj_fwd_fix=((1+proj_freq_rates_fix*days_freqT2_fix/base_fix)/(1+c(0,proj_freq_rates_fix[-lfreq_fix])*c(0,days_freqT2_fix[-lfreq_fix])/base_fix)-1)*base_fix/delta_fix
  proj_fwd_float=((1+proj_freq_rates_float*days_freqT2_float/base_float)/(1+c(0,proj_freq_rates_float[-lfreq_float])*c(0,days_freqT2_float[-lfreq_float])/base_float)-1)*base_float/delta_float

  df_fix = 1/(1 + disc_ratesT0[(days_freqT2_fix+liq)] * (days_freqT2_fix+liq)/base_disc)#Discont Factors to Valuation Date
  cum_proj_fix=cumsum(proj_fwd_fix*(delta_fix/base_fix)*df_fix)

  df_float = 1/(1 + disc_ratesT0[(days_freqT2_float+liq)] * (days_freqT2_float+liq)/base_disc)#Discont Factors to Valuation Date
  cum_proj_float=cumsum(proj_fwd_float*(delta_float/base_float)*df_float)


  if(basis_float==1){cum_basisDF = cumsum((delta_float/base_float) * df_float)
  days_freqT2_basis=days_freqT2_float
  ind_float=1}
  if(basis_float==0){cum_basisDF = cumsum((delta_fix/base_fix) * df_fix)
  days_freqT2_basis=days_freqT2_fix
  ind_float=-1}

  posBasis=match(basis_days[-c(1:k)], days_freqT2_basis)

  swap_pr=ind_float*basis_rates[-c(1:k)]*cum_basisDF[posBasis]+cum_proj_float[pos_float]-cum_proj_fix[pos_fix]

  dif_sqrt = t(swap_pr) %*% swap_pr
  return(dif_sqrt)
}
