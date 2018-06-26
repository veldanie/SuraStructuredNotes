ticket_tenor<-function(date_ini, days, tk_st=c("ON","1W", "2W", "3W")){
  if(as.numeric(format(date_ini,format="%d"))>=24){aj=10}else if(as.numeric(format(date_ini,format="%d"))<=6){aj=-10}else{aj=0}
  vto=date_ini+days-aj
  tenor_m= 12*(as.numeric(format(vto, format="%Y"))-as.numeric(format(date_ini, format="%Y")))+as.numeric(format(vto, format="%m"))-as.numeric(format(date_ini, format="%m"))
  tk_m=paste0(tenor_m,"M");posten=match(seq(12,tail(tenor_m,1),12),tenor_m);posten=posten[!is.na(posten)]
  tk_m[posten]=paste0(tenor_m[posten]/12,"Y")
  if(length(which(tenor_m==1))>1){
    pos0=setdiff(which(tenor_m==0|tenor_m==1),max(which(tenor_m==1)))}else{pos0=which(tenor_m==0)}
  tk_m[pos0]=tk_st[pos0]
  return(tk_m)
}
