bs_digital <-
function (spot, strike, v, r, rf, t, c_p = "call", payoff=1, pago_dom=1,rate_type="nominal") 
  {
    cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p == 
            "C" | c_p == "Call") * 2 - 1
    r_c = r
    rf_c= rf
    if (substr(rate_type, 1, 1) == "n") {
      r_c = log(1 + r * t)/t
      rf_c = log(1 + rf * t)/t
    }
    if (substr(rate_type, 1, 1) == "e") {
      r_c = log(1 + r)
      rf_c = log(1 + rf)
    }
    d1 = (log(spot/strike) + (r_c - rf_c + v^2/2) * t)/(v * sqrt(t))
    
    d2 = d1 - v * sqrt(t)
    if (pago_dom==1){price_bin = exp(-r_c * t) * pnorm(cp * d2)}else{price_bin = spot*exp(-rf_c * t) * pnorm(cp * d1)}
    return(payoff*price_bin)
  }
