bs_vanilla <-function (spot, strike, v, r, rf, tn, c_p = "call", rate_type = "nominal") 
{
  price = 0
  r_c = r
  rf_c = rf
  
  cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p == "C" | c_p == "Call") * 2 - 1
  if (tn <= 0) {
    price = (cp == 1) * (spot - strike) * (spot > strike) + (strike - spot) * (cp == -1) * (strike > spot)
  }
  if (tn > 0) {
    if (substr(rate_type, 1, 1) == "n") {
      r_c = log(1 + r * tn)/tn
      rf_c = log(1 + rf * tn)/tn
    }
    if (substr(rate_type, 1, 1) == "e") {
      r_c = log(1 + r)
      rf_c = log(1 + rf)
    }
    d1 = (log(spot/strike) + (r_c - rf_c + v^2/2) * tn)/(v * sqrt(tn))
    d2 = d1 - v * sqrt(tn)
    price = cp * spot * exp(-rf_c* tn) * pnorm(cp * d1) - cp * strike * exp(-r_c * tn) * pnorm(cp * d2)
  }
  return(price)
}
