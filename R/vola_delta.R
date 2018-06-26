vola_delta=function(spot, strike, d, r_quote, r_base, deltaFwd, data_matrix_d, base){
  #Return: Volatilidad para valoraci?n de una opci?n interpolada linealmente sobre t, e interpolada por deltas.
  volad=data_matrix_d[d,-1]
  ld=length(deltaFwd)
  tol=1
  v1=volad[1]
  while(tol>=0.00001){
    v=v1
    delta=callbs_d(spot, strike, v, r_quote, r_base, d/base)
    pos=max(which(delta<deltaFwd))
    if(pos==-Inf){v1=volad[1]-(delta-deltaFwd[1])*(volad[2]-volad[1])/(deltaFwd[2]-deltaFwd[1])}
    else if(pos==ld){v1=volad[ld]+(delta-deltaFwd[ld])*(volad[ld]-volad[ld-1])/(deltaFwd[ld]-deltaFwd[ld-1])}
    else{v1=volad[pos]+(delta-deltaFwd[pos])*(volad[pos+1]-volad[pos])/(deltaFwd[pos+1]-deltaFwd[pos])}
    tol=abs(v1-v)
  }
  return(v1)}
