last_date_month<-function (date_ini){
  date_initial = as.Date(paste0("01/", format(date_ini, "%m/%Y")), "%d/%m/%Y")
  last_date=date_initial
  for(i in 1:length(date_initial)){
    last_date[i] = tail(seq(date_initial[i], by = "month", length = 2), 1) - 1
  }
  return(last_date)
}
