imp_cstrike<-function(delta, spot, v, r, rf, t){
  r_c=log(1+r*t)/t
  rf_c=log(1+rf*t)/t
  cstrike=spot/exp(qnorm(delta)*v*sqrt(t)-(r_c-rf_c+v^2/2)*t)
  return(cstrike)
}
