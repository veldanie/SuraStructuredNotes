spot_rate_t0<-function(base_curr, ref_curr, spot_rate, curve_cc_usd, curve_cc_imp, base_ini, base_fin, spot_days=2, rate_type="nominal", inter_rate_type="continua"){
  if ((base_curr != "USD") & (ref_curr != "USD")){stop("Esta funci?n aplica para pares de monedas donde la base o la referencia es USD")}
  if(substr(rate_type,1,1)!="n"){stop("Esta funci?n s?lo soporta curvas de tasas nominales")}

  days_usd=curve_cc_usd[,1]
  rates_usd=curve_cc_usd[,2]
  days_imp=curve_cc_imp[,1]
  rates_imp=curve_cc_imp[,2]
  if(substr(inter_rate_type,1,1)=="n"){
    spot_rate_usd=approx(x=days_usd, y=rates_usd, xout=spot_days)$y
    spot_rate_imp=approx(x=days_imp, y=rates_imp, xout=spot_days)$y
    fd_usd=1/(1+spot_rate_usd*spot_days/base_fin)
    fd_imp=1/(1+spot_rate_imp*spot_days/base_fin)
  }else if(substr(inter_rate_type,1,1)=="c"){
    rates_usd=log(1+rates_usd*days_usd/base_ini)*base_fin/days_usd
    rates_imp=log(1+rates_imp*days_imp/base_ini)*base_fin/days_imp
    spot_rate_usd=approx(x=days_usd, y=rates_usd, xout=spot_days)$y
    spot_rate_imp=approx(x=days_imp, y=rates_imp, xout=spot_days)$y
    fd_usd=exp(-spot_rate_usd*spot_days/base_fin)
    fd_imp=exp(-spot_rate_imp*spot_days/base_fin)
  }else if(substr(inter_rate_type,1,1)=="e"){
    rates_usd=(1+rates_usd*days_usd/base_ini)^(base_fin/days_usd)-1
    rates_imp=(1+rates_imp*days_imp/base_ini)^(base_fin/days_imp)-1
    spot_rate_usd=approx(x=days_usd, y=rates_usd, xout=spot_days)$y
    spot_rate_imp=approx(x=days_imp, y=rates_imp, xout=spot_days)$y
    fd_usd=1/(1+spot_rate_usd)^(spot_days/base_fin)
    fd_imp=1/(1+spot_rate_imp)^(spot_days/base_fin)
  }

  if (base_curr == "USD") {
    spot0 = spot_rate * fd_imp/fd_usd
  }else{
    spot0 = spot_rate * fd_usd/fd_imp
  }
  return(spot0)
}
