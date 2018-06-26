liq_t0<-function(curva_t2, liq, base){
  lc=length(curva_t2$V1)
  tenor2=curva_t2$V1
  rates2=curva_t2$V2
  rates0=rep(0,lc)

  tenor0=c(1,tenor2[2:lc]+liq)
  r1=rates2[1]

  for(i in 2:lc){
    r2=rates2[i];t2=tenor2[i]
    rates0[i]=(r1*liq+r2*t2+r1*r2*liq*t2/base)/(t2+liq)
  }
  rates0[1]=r1
  return(data.frame(V1=tenor0, V2=c(rates0)))
}

liq_tn<-function(curva_t0, liq, base){
  n=length(curva_t0)
  r_liq=rep(0,n-liq)
  r_liq[1]=curva_t0[1]
  r_liq[2:(n-liq)]=((1+curva_t0[(liq+2):n]*c((liq+2):n)/base)/(1+curva_t0[1]*liq/base)-1)*base/(c((liq+2):n)-liq)
  return(r_liq)
}
