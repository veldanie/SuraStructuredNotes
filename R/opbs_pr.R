opbs_pr=function(spot, strike, v, r, rf, t, c_p="call", tipo_tasa="nominal"){
  price=0
  r_c=r
  rf_c=rf
  cp=(c_p=="call" | c_p=="c" | c_p=="CALL" | c_p=="C" | c_p=="Call")*2-1
  if(t==0){
    price=(cp==1)*(spot-strike)*(spot>strike)+(strike-spot)*(cp==-1)*(strike>spot)
  }
  if(t>0){
    if(substr(tipo_tasa,1,1)=="n"){
      r_c=log(1+r*t)/t
      rf_c=log(1+rf*t)/t}
    if(substr(tipo_tasa,1,1)=="e"){
      r_c=log(1+r)
      rf_c=log(1+rf)}

    d1=(log(spot/strike)+(r_c-rf_c+v^2/2)*t)/(v*sqrt(t))
    d2=d1-v*sqrt(t)
    price=cp*spot*exp(-rf_c*t)*pnorm(cp*d1)-cp*strike*exp(-r_c*t)*pnorm(cp*d2)
  }
  return(price)
}
