matrix_fx_cross=function (base_curr, ref_curr, spot, decim=5)
{
  n_monedas=length(spot)
  if(length(base_curr)!=n_monedas|length(ref_curr)!=n_monedas){stop("Error:Logitud de vectores no coincide")}
  if(base_curr[1]!="USD"|ref_curr[1]!="USD"){stop("Error:La primera divisa debe ser USD/USD")}

  lab=ref_curr
  lab[base_curr!="USD"]=base_curr[base_curr!="USD"]
  n_monedas=length(spot)
  matrix_tc=matrix(rep(0,n_monedas*n_monedas),nrow=n_monedas)
  matrix_tc[,1]=(base_curr=="USD")*(1/spot)+(base_curr!="USD")*(spot)
  for(i in c(2:n_monedas)){
    matrix_tc[,i]=matrix_tc[,1]*((base_curr[i]=="USD")*spot[i]+(base_curr[i]!="USD")/spot[i])
  }
  matrix_tc=round(matrix_tc,decim)
  matrix_tc=data.frame(lab,matrix_tc)
  colnames(matrix_tc)=c("Mid",lab)
  return(matrix_tc)
}
