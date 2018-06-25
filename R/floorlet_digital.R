floorlet_digital <-
function(fwd, strike, v, r, t0, t1, N=1, rate_type="nominal", payoff=1){
  if (length(t0)!=length(t1)){
    stop("Longitud de t es diferente a la longitud de T")}  
  
  r_c=r
  if (substr(rate_type, 1, 1) == "n") {
    r_c = log(1 + r * t)/t
  }
  if (substr(rate_type, 1, 1) == "e") {
    r_c = log(1 + r)
  }
  
  d1 = (log(fwd/strike) +  (v^2/2) * t1)/(v * sqrt(t1))
  d2 = d1 - v * sqrt(t1)
  price = payoff*exp(-r_c * t1) * pnorm(-d1)
  return(price)  
}
