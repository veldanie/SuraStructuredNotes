caplet <-
function(fwd, strike, v, r, t0, t1, N=1, rate_type="nominal"){
  #t0: maduracion de la opcion.
  #t1: fecha de pago del contrato.
  #t1-t0: Prediodo de la tasa subyacente.
  #r: Tasa hasta el pago de la opcion.

  r_c=r
  if (substr(rate_type, 1, 1) == "n") {
    r_c = log(1 + r * t)/t
  }
  if (substr(rate_type, 1, 1) == "e") {
    r_c = log(1 + r)
  }

  if (length(t1)!=length(t0)){
    stop("Longitud de t0 es diferente a la longitud de t1")}
  d1 = (log(fwd/strike) +  (v^2/2) * t1)/(v * sqrt(t1))
  d2 = d1 - v * sqrt(t1)
  price = N*(t1-t0)*exp(-r_c * t1) * (fwd*pnorm(d1) -  strike * pnorm(d2))
  return(price)
}
