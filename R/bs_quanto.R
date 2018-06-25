bs_quanto <-function (spot, strike, quanto, v, vq, rho, r, rf, rq, tn, c_p = "call", rate_type="nominal")
{
  #quanto: Quanto factor.
  #vq: Volatility of pair DOM-Quanto
  #rq: Risk free rate of the Quanto Currency.
  #rho: Corr between the currency pairs FOR-DOM aand DOM-Quanto
  cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p == 
          "C" | c_p == "Call") * 2 - 1  
  r_c=r
  rf_c=rf
  rq_c=rq
  if (tn <= 0) {
    price = (cp == 1) * quanto * (spot - strike) * (spot > strike) + (cp == -1) * quanto * (strike - spot) * (strike > spot)
  }
  if(tn>0){
  if (substr(rate_type, 1, 1) == "n") {
    r_c = log(1 + r * tn)/tn
    rf_c = log(1 + rf * tn)/tn
    rq_c = log(1 + rq * tn)/tn    
  }
  if (substr(rate_type, 1, 1) == "e") {
    r_c = log(1 + r)
    rf_c = log(1 + rf)
    rq_c = log(1 + rq)
  }
  mu=r_c-rf_c-rho*v*vq 
  
  d1 = (log(spot/strike) + (mu + v^2/2) * tn)/(v * sqrt(tn))
  d2 = d1 - v * sqrt(tn)
  price = cp * quanto * exp(-rq_c * tn) * (spot * exp(mu * tn) * pnorm(cp * d1) - strike * pnorm(cp * d2))
  }
  return(price)
}
