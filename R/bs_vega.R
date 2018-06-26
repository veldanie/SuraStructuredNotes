bs_vega<-function(spot, strike, v, r, rf, t){
  r_c=log(1+r*t)/t
  rf_c=log(1+rf*t)/t

  d1=(log(spot/strike)+(r_c-rf_c+v^2/2)*t)/(v*sqrt(t))
  vega=spot*sqrt(t)*exp(-rf_c-d1^2/2)/sqrt(2*pi)
  return(vega)
}
