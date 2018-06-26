vola_strike_matrix=function (strike, d, data_matrix)
{
  #Funci?n que obtiene la volatilidad para un determinado strike, dada una matrix de Dias vs Strikes
  strikes=as.numeric(colnames(data_matrix)[-1]);ls=length(strikes)
  days=data_matrix[,1]
  vola=data_matrix[,-1]
  pos = max(which(strike > strikes))
  if (pos == -Inf) {
    vola1=approx(days, vola[,1],d)$y
    vola2=approx(days, vola[,2],d)$y
    v = vola1 - (strike - strikes[1]) * (vola2 - vola1)/abs(strikes[2] - strikes[1])
  }
  else if (pos == ls) {
    vola1=approx(days, vola[,(ld-1)],d)$y
    vola2=approx(days, vola[,ld],d)$y

    v = vola1 - (strike - strikes[(ld-1)]) * (vola2 - vola1)/abs(strikes[ld] - strikes[(ld-1)])
  }
  else {
    vola1=approx(days, vola[,pos],d)$y
    vola2=approx(days, vola[,(pos+1)],d)$y

    v = vola1 - (strike - strikes[pos]) * (vola2 - vola1)/abs(strikes[(pos+1)] - strikes[(pos)])
  }
  return(v)
}
