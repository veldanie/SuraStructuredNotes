pr_forward=function(spot, days, rates_dom, rates_for, base_dom=360, base_for=360, tipo_tasa="nom"){
  #precio forward usando tasas nominal.
  if(length(days)!=length(rates_dom)|length(days)!=length(rates_for)){stop("Dimensi?n de los vectores no coincide. Favor revisar!")}
  if(substr(tipo_tasa,1,1)=="n"|substr(tipo_tasa,1,1)=="N"){
    pr_fwd=spot*(1+rates_dom*days/base_dom)/(1+rates_for*days/base_for)}
  if(substr(tipo_tasa,1,1)=="e"|substr(tipo_tasa,1,1)=="E"){
    pr_fwd=spot*((1+rates_dom)^(days/base_dom))/((1+rates_for)^(days/base_for))}
  if(substr(tipo_tasa,1,1)=="c"|substr(tipo_tasa,1,1)=="C"){
    pr_fwd=spot*exp(rates_dom*days/base_dom-rates_for*days/base_for)}

  return(list(pr=pr_fwd,pips=pr_fwd-spot))
}
