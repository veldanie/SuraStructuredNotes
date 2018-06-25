cc_price <-
function (days, rates, base=360, rate_type="nominal"){
#Precio de bonos cero cupon a partir de tasas NOMINALES
  rates_c = rates
  tn=days/base

  if (substr(rate_type, 1, 1) == "n") {
    rates_c = log(1 + rates * tn)/tn
  }
  if (substr(rate_type, 1, 1) == "e") {
    rates_c = log(1 + rates)
  }
  df=exp(-rates_c*tn)
  return(df)
}
