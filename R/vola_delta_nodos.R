vola_delta_nodos=function(spot, strikes, volad, rcop, rusd, t, deltaFwd, base, fwd=TRUE){
  #Return: Volatilidad para los nodos deltaFwd.
  ld=length(deltaFwd)
  delta=callbs_d(spot, strikes, volad, r, rf, t, fwd)
  vola_delta=c()
  for(i in c(1:ld)){
    pos=max(which(deltaFwd[i]<delta))
    if(pos==-Inf){v1=volad[1]-(deltaFwd[i]-delta[1])*(volad[2]-volad[1])/(delta[2]-delta[1])}
    else if(pos==ld){v1=volad[ld]+(deltaFwd[i]-delta[ld])*(volad[ld]-volad[ld-1])/(delta[ld]-delta[ld-1])}
    else{v1=volad[pos]+(deltaFwd[i]-delta[pos])*(volad[pos+1]-volad[pos])/(delta[pos+1]-delta[pos])}
    vola_delta=c(vola_delta,v1)
  }
  return(vola_delta)}
