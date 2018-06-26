df_nom=function(tasa_nom, days, base){
  df_nom=1/(1+tasa_nom*days/base)
  return(data.frame(V1=days,V2=df_nom))
}
