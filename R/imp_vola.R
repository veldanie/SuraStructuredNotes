imp_vola<-function(price, spot, strike, r, rf, tn, c_p="CALL",tipo_tasa = "nominal",interv=c(0,2)){
  vol=uniroot(vola_obj,interval=interv, price=price, spot=spot, strike=strike, r=r, rf=rf, tn=tn, c_p=c_p,tipo_tasa=tipo_tasa)$root
  return(vol)
}
