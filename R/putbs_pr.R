putbs_pr=function(spot, strike, v, r, rf, t){
  #v: Volatility
  r_c=log(1+r*t)/t
  rf_c=log(1+rf*t)/t
  d1=(log(spot/strike)+(r_c-rf_c+v^2/2)*t)/(v*sqrt(t))
  d2=d1-v*sqrt(t)
  price=strike*exp(-r_c*t)*pnorm(-d2)-spot*exp(-rf_c*t)*pnorm(-d1)
  return(price)
}
