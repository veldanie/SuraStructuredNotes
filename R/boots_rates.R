boots_rates=function(curva, base, cc_days, freq, days_freq){
  tenor=curva$V1
  swp_rates=curva$V2
  n=length(swp_rates)
  days_freq=days_freq[days_freq<=tenor[n]]

  swpcc_curve=data.frame(V1=curva$V1[curva$V1<=cc_days],V2=curva$V2[1:length(curva$V1[curva$V1<=cc_days])])

  swpd_rates=curva_est(curva)
  swpfreq_rates=swpd_rates[days_freq]
  DF=rep(0,length(swpfreq_rates))

  k=sum((days_freq<=cc_days))
  for (i in 1:k){
    DF[i]=1/(1+swpfreq_rates[i]*days_freq[i]/base)
  }

  lf=length(days_freq)
  for (j in (k+1):lf){
    daysj=days_freq[1:j]
    deltai=(daysj-c(0,daysj[1:(length(daysj)-1)]))/base
    DF[j]=(1-swpfreq_rates[j]*sum(deltai[1:j-1]*DF[1:j-1]))/(1+swpfreq_rates[j]*deltai[j])
  }

  cc_rates=(1/DF-1)*(base/days_freq)
  cc_curve_freq=data.frame(V1=c(1,days_freq), V2=c(swp_rates[1],cc_rates))
  cc_curve=subset(merge(cc_curve_freq, swpcc_curve, all=TRUE), !duplicated(V1))
  return(cc_curve)
}
