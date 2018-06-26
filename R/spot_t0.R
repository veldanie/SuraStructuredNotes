spot_t0=function(base_curr, ref_curr, spot_t1, cc_usd, cc_imp, base){
  if ((base_curr != "USD") & (ref_curr != "USD"))
    stop("Esta funci?n aplica para pares de monedas donde la base o la referencia es USD")
  if(base_curr=="USD"){
    spot0=spot_t1*(1+cc_imp/base)/(1+cc_usd/base)
  }else{
    spot0=spot_t1*(1+cc_usd/base)/(1+cc_imp/base)}
  return(spot0)
}
