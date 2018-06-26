bs_d<-function(spot, strike, v, r, rf, t,c_p="call", fwd=TRUE){
  cp=(c_p=="call" | c_p=="c" | c_p=="CALL" | c_p=="C" | c_p=="Call")*2-1

  r_c=log(1+r*t)/t
  rf_c=log(1+rf*t)/t
  d1=(log(spot/strike)+(r_c-rf_c+v^2/2)*t)/(v*sqrt(t))
  if(fwd==TRUE){delta=cp*pnorm(cp*d1)} else {delta=cp*exp(-rf_c*t)*pnorm(cp*d1)}
  return(delta)
}
