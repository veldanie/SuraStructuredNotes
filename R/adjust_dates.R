adjust_dates<-function(dates, sett_dates, n_sett_days){
  new_dates=dates
  pos_dates_na=is.na(match(dates,sett_dates))
  if(n_sett_days==0){
    new_dates[pos_dates_na]=sett_dates[findInterval(dates[pos_dates_na],sett_dates)+1]
    new_dates[!pos_dates_na]==sett_dates[match(dates[!pos_dates_na],sett_dates)]
  }
  if(n_sett_days>0){
    new_dates=sett_dates[findInterval(dates,sett_dates)+n_sett_days]
  }
  return(new_dates)
}
