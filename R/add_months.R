add_months<-function(date_ini,months_vector){
  month_dates=seq(date_ini,by="month", length=tail(months_vector,1)+1)[months_vector+1]
  delta_m=as.numeric(format(month_dates,"%m"))-as.numeric(format(date_ini,"%m"))
  delta_m[delta_m<0]=delta_m[delta_m<0]+12
  month_dates[(delta_m-months_vector)>0]=month_dates[(delta_m-months_vector)>0]-as.numeric(format(month_dates[(delta_m-months_vector)>0],"%d"))
  return(month_dates)
}
