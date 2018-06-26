dif_price_boots=function (param, curva, base, cc_days, days_freq)
{

  swap_days = curva[, 1]
  swap_rates = curva[, 2]
  n = length(swap_rates)

  days_freq = days_freq[days_freq <= (tail(swap_days,1)+15)]
  k = sum(swap_days <= cc_days)

  ccfreq_rates = approx_extrap(x = swap_days, y =c(swap_rates[c(1:k)], param), xout = days_freq)$y
  fd_est = 1/(1 + ccfreq_rates * days_freq/base)
  delta = (days_freq - c(0, days_freq[-length(days_freq)]))/base
  deltaFD = cumsum(delta * fd_est)
  pos = match(swap_days[-c(1:k)], days_freq)
  swap_est = (1 - fd_est[pos])/deltaFD[pos]
  dif = swap_est - curva[-c(1:k), 2]
  dif_sqrt = t(dif) %*% dif
  return(dif_sqrt)
}
