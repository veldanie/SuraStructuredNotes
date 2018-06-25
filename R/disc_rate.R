disc_rate <-
function (cc_price, days, base=360, rate_type="nominal"){
  #Precio de bonos cero cupon a partir de tasas NOMINALES
  rate=0
  tn=days/base
  if (substr(rate_type, 1, 1) == "c") {
    rate = -log(cc_price)/tn
  }
  if (substr(rate_type, 1, 1) == "n") {
    rate = (1/cc_price-1)/tn
  }
  if (substr(rate_type, 1, 1) == "e") {
    rate = (1/cc_price)^(1/tn)-1
  }
  return(rate)
}
