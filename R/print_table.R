print_table<-function(date_ini, curve, id){
  days=curve[,1]
  tk_st=c("ON", "1W", "2W", "3W")
  if(any(substr(id,1,1)==c("F","I"))){
    tk_st=c("ON","SP")
    if((days[3]-days[2])<5){tk_st=c(tk_st,"SN")}
  }
  tk=ticket_tenor(date_ini, days, tk_st=tk_st)
  tk[is.na(tk)]=c("1W","2W","3W")[1:sum(is.na(tk))]
  return(data.frame(ID=id,TENOR=tk, DIAS=days, TASA=curve[,2]))
}
