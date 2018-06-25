vol_extract <-
function(d, strike, spot, r=NULL, rf=NULL, base=360, vol_matrix, quote_delta=TRUE, quote_bfrr=TRUE, delta_type=1, rate_type="nominal", extrap=FALSE){
  #Funcion que permite extraer un dato de volatilidad de una matriz por deltas o por strikes. La interpolaci?n es lineal por tiempo y por strikes.
  #d:=vector de dias de las opciones buscadas.
  #strike:=vector de strikes de las opciones buscadas.
  #vola_matrix:= Matriz de volatilidades. Debe tener HEADER. Si es Matriz deltas {header="d", 0.9,...,0.1}. Si es matriz strikes {header="d", strikes}
  #delta:=Funcion indicadora de la estructura de la Matriz. Si delta=1{matriz de deltas, e.g. USDCOP}. Si delta=0{matriz de strikes, e.g. SPX}
  #quote_bfrr:= Funci?n indicadora de la forma de cotizaci?n. Si delta=1{matriz de deltas, e.g. USDCOP}. Si delta=0{matriz de strikes, e.g. SPX}
  #delta_type:=tipo de cotizacion delta. 1=Delta Spot sin prima, 2=Delta Forward sin prima, 3=Delta Spot con prima, 4=Delta Forwards con prima
  if(dim(vol_matrix)[2]==2){
    if(extrap==TRUE){v=approx_extrap(x=vol_matrix[,1],y=vol_matrix[,2],xout=d)$y}
    if(extrap==FALSE){v=approx(x=vol_matrix[,1],y=vol_matrix[,2],xout=d, yleft=vol_matrix[1,2], yright=tail(vol_matrix[,2],1))$y
    }
  }else{

  if(!any(delta_type==c(1,2))){stop("Actualmente s?lo se soporta delta_type 1 y 2.")}
  r_c = r
  rf_c= rf
  tn=d/base

  if (substr(rate_type, 1, 1) == "n") {
    r_c = log(1 + r * tn)/tn
    rf_c = log(1 + rf * tn)/tn
  }
  if (substr(rate_type, 1, 1) == "e") {
    r_c = log(1 + r)
    rf_c = log(1 + rf)
  }
  ##Conversi?n de matriz BFRR a matriz de Deltas:
  if(quote_delta==TRUE){
    if(quote_bfrr==TRUE){
      vol_matrix=bfrr_to_delta(vol_matrix, header_vola=c("d", 0.9,0.75,0.5,0.25,0.1))
    }
    volad=matrix_approx(vol_matrix,d, extrap=extrap)[-1]
    delta_ref=as.numeric(colnames(vol_matrix)[-1])
    adjust=1

    if(delta_type==1){adjust=exp(rf_c*tn)}
    imp_str = c(imp_pstrike(0.1*adjust, spot, volad[1], r, rf, tn),
                imp_pstrike(0.25*adjust, spot, volad[2], r, rf, tn),
                spot*exp((r_c-rf_c+0.5*volad[3]^2)*tn),
                imp_cstrike(0.25*adjust, spot, volad[4], r, rf, tn),
                imp_cstrike(0.1*adjust, spot, volad[5], r, rf, tn))
  }
  if(quote_delta==FALSE){
    imp_str=as.numeric(colnames(vol_matrix)[-1])
    volad=as.matrix(matrix_approx(vol_matrix,d,extrap=extrap))[,-1]
  }
  ld=length(imp_str)
  ldays=length(d)
  pos = max(which(strike > imp_str))
  if(ldays==1){
  if (pos == -Inf) {
    v=volad[1]
    if(extrap==TRUE){
    v = volad[1] + (strike - imp_str[1]) * (volad[2] - volad[1])/(imp_str[2] - imp_str[1])}
  }else if (pos == ld) {
    v=volad[ld]
    if(extrap==TRUE){
    v = volad[ld] + (strike - imp_str[ld]) * (volad[ld] - volad[ld - 1])/(imp_str[ld] - imp_str[ld - 1])}
  }else {
    v = volad[pos] + (strike - imp_str[pos]) * (volad[pos + 1] - volad[pos])/(imp_str[pos + 1] - imp_str[pos])
  }}
  if(ldays>1){
  v=rep(0,ldays)
  for(i in c(1:ldays)){
    if (pos == -Inf) {
      v[i] = volad[i,1] - (strike - imp_str[1]) * (volad[i,2] - volad[i,1])/abs(imp_str[2] - imp_str[1])
    }else if (pos == ld) {
      v[i] = volad[i,ld] + (strike - imp_str[ld]) * (volad[i,ld] - volad[i,(ld - 1)])/(imp_str[ld] - imp_str[ld - 1])
    }else {
      v[i] = volad[i,pos] + (strike - imp_str[pos]) * (volad[i,(pos + 1)] - volad[i,pos])/(imp_str[pos + 1] - imp_str[pos])
    }}}
  }
  return(as.numeric(v))
}
