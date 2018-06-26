basis<-function(cc_irs, cc_ccs, base, cc_days, days_freq){

  n=min(length(cc_irs), length(cc_ccs))
  days_freq=days_freq[days_freq<=n]

  irsfreq_rates=cc_irs[days_freq]
  ccsfreq_rates=cc_ccs[days_freq]

  basis=rep(0,sum(days_freq>cc_days))
  lf=length(days_freq)

  k=sum((days_freq<=cc_days))
  delta=(days_freq-c(0,days_freq[1:(lf-1)]))/base

  DFirs=1/(1+irsfreq_rates*days_freq/base)
  DFccs=1/(1+ccsfreq_rates*days_freq/base)
  fwdirs=(c(1, DFirs[1:(lf-1)])/DFirs-1)*(1/delta)

  for (j in (k+1):lf){
    daysj=days_freq[1:j]
    deltaj=delta[1:j]
    ld=length(daysj)
    basis[j-k]=(1-sum(deltaj[1:j]*DFccs[1:j]*fwdirs[1:j])-DFccs[j])/sum(deltaj*DFccs[1:j])
  }
  return(data.frame(V1=days_freq[days_freq>cc_days],V2=basis))
}
