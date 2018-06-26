days_count_tenors_ndf<-function (date_ini, sett_dates, tenors, t_plus = 0){
  ntenors = length(tenors)
  value_dates_tenors = rep(date_ini, ntenors)
  nch = nchar(tenors)
  ch_last = substr(tenors, nch, nch)
  posw = which(ch_last == "W")
  posm = which(ch_last == "M")
  posy = which(ch_last == "Y")
  poson = setdiff(1:ntenors, c(posw, posm, posy))
  pos_date=findInterval(date_ini, sett_dates)
  sett_ini=sett_dates[pos_date+t_plus]
  liq_d=as.numeric(sett_ini-date_ini)

  if (length(poson) != 0) {
    pos_dates = pos_date + 1
    value_dates_tenors[poson]= sett_dates[pos_dates]
  }
  if (length(posw) != 0) {
    idw = gsub("W", "", tenors[posw], fixed = T)
    idw[idw == "S"] = "1"
    days_v = as.numeric(idw) * 7
    pos_dates = findInterval(date_ini + days_v, sett_dates)
    pos_dates[is.na(match(date_ini + days_v, sett_dates))] = pos_dates[is.na(match(date_ini + days_v, sett_dates))] + 1
    value_dates_tenors[posw] = sett_dates[pos_dates]
  }

  end_of_month=as.numeric(format(sett_ini+1,"%d"))==1
  if (length(posm) != 0) {
    months_v = as.numeric(gsub("M", "", tenors[posm], fixed = T))
    dates_month = add_months(sett_ini, months_v)
    if(end_of_month){dates_month=last_date_month(dates_month)}
    pos_dates = findInterval(dates_month, sett_dates)
    nomatch = is.na(match(dates_month, sett_dates))
    delta_month = as.numeric(format(dates_month[nomatch], "%m")) - as.numeric(format(sett_dates[pos_dates[nomatch] + 1], "%m"))
    pos_dates[nomatch] = pos_dates[nomatch] + (delta_month == 0)
    value_dates_tenors[posm] = sett_dates[pos_dates]
  }
  if (length(posy) != 0) {
    months_v = as.numeric(gsub("Y", "", tenors[posy], fixed = T)) *12
    dates_month = add_months(sett_ini, months_v)
    if(end_of_month){dates_month=last_date_month(dates_month)}
    pos_dates = findInterval(dates_month, sett_dates)
    nomatch = is.na(match(dates_month, sett_dates))
    delta_month = as.numeric(format(dates_month[nomatch], "%m")) - as.numeric(format(sett_dates[pos_dates[nomatch] + 1], "%m"))
    pos_dates[nomatch] = pos_dates[nomatch] + (delta_month == 0)
    value_dates_tenors[posy] = sett_dates[pos_dates]
  }
  fix_dates_tenors = value_dates_tenors
  fix_dates_tenors[c(posm,posy)]=sett_dates[match(value_dates_tenors[c(posm,posy)],sett_dates)-2]
  fix_dayst0 = as.numeric(fix_dates_tenors - date_ini)
  fix_dayst2 = as.numeric(fix_dates_tenors - sett_ini)
  value_dayst0 = as.numeric(value_dates_tenors - date_ini)
  value_dayst2 = as.numeric(value_dates_tenors - sett_ini)

  if ((length(poson)+length(posw)) > 0) {
    fix_dayst2 = c(fix_dayst0[c(poson, posw)], as.numeric(fix_dayst2[-c(poson, posw)]))
    value_dayst2 = c(value_dayst0[c(poson, posw)], as.numeric(value_dayst2[-c(poson, posw)]))
  }

  return(list(fix_dates = fix_dates_tenors, value_dates = value_dates_tenors, fix_dayst0 = fix_dayst0,
              fix_dayst2 = fix_dayst2, value_dayst0 = value_dayst0, value_dayst2 = value_dayst2))
}
