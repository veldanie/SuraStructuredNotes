vol_term_structure <-
function(series, days=c(30,180,360,720), days_year=360){
  
  dates=series[,1]
  index=series[order(dates),2]
  ls=length(dates)
  vol=rep(0,length(days))
  for(i in 1:length(days)){
    di=min(ls,days[i])
    indexi=tail(index,di)
    ret=diff(indexi)/indexi[1:(di-1)]
    vol[i]=sd(ret[ret=!0])*sqrt(days_year)    
  }
  pos_vol=!is.na(vol)
  vol_curve=data.frame(DAYS=days[pos_vol], VOL=vol[pos_vol])
  return(list(vol_curve=vol_curve))
}
