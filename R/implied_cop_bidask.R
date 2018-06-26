implied_cop_bidask<-function(cc_usd, fwds, TRM, base){
  lf=length(fwds$V1)
  days=c(1:lf)
  mid_usd=cc_usd$V1[1:lf]
  bid_usd=cc_usd$V2[1:lf]
  ask_usd=cc_usd$V3[1:lf]

  mid_fwd=fwds$V1[1:lf]
  bid_fwd=fwds$V2[1:lf]
  ask_fwd=fwds$V3[1:lf]

  imp_mid=((1+mid_usd*days/base)*(mid_fwd/TRM+1)-1)*base/days
  imp_bid=((1+ask_usd*days/base)*(bid_fwd/TRM+1)-1)*base/days
  imp_ask=((1+bid_usd*days/base)*(ask_fwd/TRM+1)-1)*base/days
  imp_rates_bidask=data.frame(V1=imp_mid,V2=imp_bid, V3=imp_ask)
  return(imp_rates_bidask)
}
