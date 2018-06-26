cc_basis<-function(cc_irs, implied_rate, basis_spr, base, cc_days, days_freq){

  n=min(length(cc_irs), length(basis_spr))
  days_freq=days_freq[days_freq<=n]
  imp_days_freq=days_freq[days_freq<=cc_days]

  irsfreq_rates=cc_irs[days_freq]
  impliedfreq_rates=implied_rate[imp_days_freq]
  basisfreq_rates=basis_spr[days_freq[days_freq>cc_days]]

  lf=length(days_freq)
  li=length(imp_days_freq)
  basis_df=rep(0,lf)

  delta=(days_freq-c(0,days_freq[1:(lf-1)]))/base

  DFirs=1/(1+irsfreq_rates*days_freq/base)
  fwdirs=(c(1, DFirs[1:(lf-1)])/DFirs-1)*(1/delta)

  DFimplied=1/(1+impliedfreq_rates*imp_days_freq/base)
  if(li>0){basis_df[1:li]=DFimplied}

  for (j in (li+1):lf){
    daysj=days_freq[1:j]
    deltaj=delta[1:j]
    ld=length(daysj)
    basis_df[j]=(1-sum(deltaj[1:(j-1)]*basis_df[1:(j-1)]*(fwdirs[1:(j-1)]+basisfreq_rates[j-li])))/(1+deltaj[j]*(fwdirs[j]+basisfreq_rates[j-li]))
  }
  cc_basis=(1/basis_df-1)*base/days_freq

  return(data.frame(V1=days_freq,V2=cc_basis))
}
