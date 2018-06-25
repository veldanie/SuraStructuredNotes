floorlet <-
function(fwd, strike, v, r, t0, t1, N=1){
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
  price = N*(t1-t0)*exp(-r_c * t1) * (strike*pnorm(-d2) - fwd * pnorm(-d1))
  return(price)  
}
