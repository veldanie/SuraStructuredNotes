imp_rates_usd<-function(base_curr, ref_curr, spot, factor_fwd, cc_usd, fwds, base){

  if (any(base_curr!= "USD") & any(ref_curr != "USD"))
    stop("Esta funci?n aplica para pares de monedas donde la base o la referencia sea USD")

  lf=length(fwds)
  days=c(1:lf)

  mid_usd=cc_usd[1:lf]

  if (base_curr=="USD"){
    imp_mid=((1+mid_usd*days/base)*((fwds/factor_fwd)/spot+1)-1)*base/days
  }else if (ref_curr=="USD"){
    imp_mid=((1+mid_usd*days/base)/((fwds/factor_fwd)/spot+1)-1)*base/days
  }
  return(imp_mid)
}
