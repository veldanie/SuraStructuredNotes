dif_price_boots_irs<-function (param, curveT2, base, cc_days, days_freq, liq_d) {

  swap_days = curveT2[, 1]
  swap_rates = curveT2[, 2]
  n = length(swap_rates)
  days_freq = days_freq[days_freq <= (tail(swap_days, 1) + 15)]

  k = sum(swap_days <= cc_days)

  ccfreq_rates = approx_extrap(x = swap_days, y = c(swap_rates[c(1:k)], param), xout = days_freq)$y
  lfreq=length(ccfreq_rates)
  delta = (days_freq - c(0, days_freq[-lfreq]))/base

  proj_fwd = ((1 + ccfreq_rates * days_freq/base)/(1 + c(0, ccfreq_rates[-lfreq]) * c(0, days_freq[-lfreq])/base) - 1) /delta

  ccfreq_rates_t0=liq_t0(data.frame(V1=c(1,days_freq), V2=c(swap_rates[1],ccfreq_rates)), liq_d, base)[-1,2]

  fd_est = 1/(1 + ccfreq_rates_t0 * (days_freq+liq_d)/base)
  deltaFD = cumsum(delta * fd_est)
  pos = match(swap_days[-c(1:k)], days_freq)
  float_leg=cumsum(proj_fwd*fd_est*delta)
  swap_est = float_leg[pos]/deltaFD[pos]
  dif = swap_est - curveT2[-c(1:k), 2]
  dif_sqrt = t(dif) %*% dif
  return(dif_sqrt)
}
