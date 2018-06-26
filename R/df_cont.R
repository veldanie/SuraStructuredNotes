df_cont=function(tasa_cont, days, base){
  df_cont=exp(-tasa_cont*days/base)
  return(data.frame(V1=days,V2=df_cont))
}
