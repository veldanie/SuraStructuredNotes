putbs_d<-function(spot, strike, v, r, rf, t, fwd=TRUE){
  r_c=log(1+r*t)/t
  rf_c=log(1+rf*t)/t
  d1=(log(spot/strike)+(r_c-rf_c+v^2/2)*t)/(v*sqrt(t))
  #delta=exp(-r_c*t)*abs(pnorm(d1)-1)
  if(fwd==TRUE){delta=-pnorm(-d1)} else {delta=-exp(-rf_c*t)*pnorm(-d1)}
  return(delta)
}
