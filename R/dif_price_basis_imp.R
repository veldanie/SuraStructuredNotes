dif_price_basis_imp=function(param, disc_curveT0_dom, proj_curveT2_dom, base, cc_days, days_freq, liq_dom){
  daysT0 = disc_curveT0_dom[,1]
  daysT2 = disc_curveT0_dom[-1,1]-liq_dom
  days_freq = days_freq[days_freq <= (tail(daysT2, 1)+15)]
  lfreq=length(days_freq)
  cc_ratesT0=disc_curveT0_dom[,2]
  #n = length(swap_rates)

  k = sum(daysT0 <= cc_days)
  cc_freq_rates = approx_extrap(x = daysT0, y = cc_ratesT0, xout = days_freq+liq_dom)$y
  df = 1/(1 + cc_freq_rates * (days_freq+liq_dom)/base)

  delta = (days_freq - c(0, days_freq[-length(days_freq)]))/base

  proj_freq_rates=approx_extrap(x = proj_curveT2_dom[,1], y = proj_curveT2_dom[,2], xout = days_freq)$y
  proj_fwd = ((1 + proj_freq_rates * days_freq/base)/(1 +c(0, proj_freq_rates[-lfreq]) * c(0, days_freq[-lfreq])/base) - 1) * 1/delta

  cum_deltadf = cumsum(delta * df)
  cum_proj_fwd=cumsum(proj_fwd *delta * df)

  pos = match(daysT2, days_freq)

  basis_est =  param * cum_deltadf[pos] + cum_proj_fwd[pos] +df[pos]- 1

  dif_sqrt = t(basis_est) %*% basis_est
  return(dif_sqrt)
}
