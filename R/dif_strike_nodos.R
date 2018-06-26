dif_strike_nodos <- function(strike, spot, strike1, strike2, vola1, vola2, r_quote, r_base, t, deltaFwd, fwd=TRUE){
  vola=approx_extrap(c(strike1, strike2), c(vola1, vola2), strike)$y
  dif=(abs(deltaFwd-callbs_d(spot, strike, vola, r_quote, r_base, t, fwd)))^2
  return(dif)
}
