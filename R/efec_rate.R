efec_rate=function (tasa, d, base_ini, base_fin, tipo_ini = "continua"){
  tasa_efe=NULL
  t_ini = d/base_ini
  t_fin = d/base_fin
  if(substr(tipo_ini,1,1) == "C" |substr(tipo_ini,1,1) == "c"){tasa_efe =(exp(tasa * t_ini)^(1/t_fin)) - 1}
  if(substr(tipo_ini,1,1) == "N" |substr(tipo_ini,1,1) == "n"){tasa_efe =((1 + tasa*t_ini)^(1/t_fin))-1}
  return(tasa_efe)
}
