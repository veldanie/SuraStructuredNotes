nom_rate<-function (tasa, d, base_ini, base_fin, tipo_ini = "continua")
{
  tasa_nom=NULL
  t_ini = d/base_ini
  t_fin = d/base_fin
  if(substr(tipo_ini,1,1) == "C" |substr(tipo_ini,1,1) == "c"){tasa_nom =(exp(tasa * t_ini) - 1)/t_fin}
  if(substr(tipo_ini,1,1) == "E" |substr(tipo_ini,1,1) == "e"){tasa_nom =((1 + tasa)^(t_ini)-1)/t_fin}
  return(tasa_nom)
}
