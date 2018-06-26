df_efec=function(tasa_efec, days, base){
  df_efec=1/((1+tasa_efec)^(days/base))
  return(data.frame(V1=days,V2=df_efec))
}
