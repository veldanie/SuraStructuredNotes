cont_rate=function (tasa, d, base_ini, base_fin, tipo_ini = "nominal"){
  tasa_cont=NULL
  t_ini = d/base_ini
  t_fin = d/base_fin
  if(substr(tipo_ini,1,1) == "E" |substr(tipo_ini,1,1) == "e"){tasa_cont =log(1+tasa)*t_ini/t_fin}
  if(substr(tipo_ini,1,1) == "N" |substr(tipo_ini,1,1) == "n"){tasa_cont =log(1+tasa*t_ini)/t_fin}
  return(tasa_cont)
}
