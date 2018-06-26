imp_pstrike<-function(delta, spot, v, r, rf, t){
  r_c=log(1+r*t)/t
  rf_c=log(1+rf*t)/t

  pstrike=spot/exp(qnorm(1-delta)*v*sqrt(t)-(r_c-rf_c+v^2/2)*t)
  return(pstrike)
}
