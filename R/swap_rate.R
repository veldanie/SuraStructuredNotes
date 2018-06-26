swap_rate=function(cc_curve, base, cc_days, days_freq, days_swap){

  maxd=min(tail(days_swap,1), tail(cc_curve$V1,1))
  days_freq = days_freq[days_freq <= maxd]
  cc_curve_d=curva_est(cc_curve)
  cc_rates_freq=cc_curve_d[days_freq]

  fd_freq=1/(1+cc_rates_freq*days_freq/base)
  delta=(days_freq-c(0,days_freq[-length(days_freq)]))/base

  swap_rates=rep(0,sum(cc_curve$V1<=cc_days)+length(days_swap))
  for (i in c(1:sum(cc_curve$V1<=cc_days))){
    swap_rates[i]=cc_curve$V2[i]
  }
  swap_rates_freq=rep(0,length(days_freq))
  deltaFd=cumsum(delta*fd_freq)
  for (j in c(1:(length(days_freq)))){
    swap_rates_freq[j]=(1-fd_freq[j])/deltaFd[j]
  }
  days=c(cc_curve$V1[c(1:i)] ,days_swap)
  swap_rates[-c(1:i)]=swap_rates_freq[match(days_swap,days_freq)]
  return(data.frame(V1=days,V2=swap_rates))
}
