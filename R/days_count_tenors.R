days_count_tenors<-function(date_ini, sett_dates, tenors, liq_d=0, conv="F"){
  ##conv:= Convenci?n de conteo (F=Following, MD=Modified following)
  ntenors = length(tenors)
  dates_tenors = rep(date_ini, ntenors)
  nch = nchar(tenors)
  ch_last = substr(tenors, nch, nch)
  posw = which(ch_last == "W")
  posm = which(ch_last == "M")
  posy = which(ch_last == "Y")
  poson = setdiff(1:ntenors, c(posw, posm, posy))
  sett_ini = date_ini + liq_d
  if (length(poson) != 0) {
    pos_dates = findInterval(date_ini, sett_dates) + 1
    dates_tenors[poson] = sett_dates[pos_dates]
  }
  if (length(posw) != 0) {
    idw = gsub("W", "", tenors[posw], fixed = T)
    idw[idw == "S"] = "1"
    days_v = as.numeric(idw) * 7
    pos_dates = findInterval(date_ini + days_v, sett_dates)
    pos_dates[is.na(match(date_ini + days_v, sett_dates))] = pos_dates[is.na(match(date_ini + days_v, sett_dates))] + 1
    dates_tenors[posw] = sett_dates[pos_dates]
  }
  if (length(posm) != 0) {
    months_v = as.numeric(gsub("M", "", tenors[posm], fixed = T))
    dates_month = add_months(sett_ini, months_v)
    pos_dates = findInterval(dates_month, sett_dates)
    nomatch = is.na(match(dates_month, sett_dates))
    if (conv == "F") {
      pos_dates[nomatch] = pos_dates[nomatch] + 1
    }
    else if (conv == "MF") {
      delta_month = as.numeric(format(dates_month[nomatch], "%m")) - as.numeric(format(sett_dates[pos_dates[nomatch] + 1], "%m"))
      pos_dates[nomatch] = pos_dates[nomatch] + (delta_month == 0)
    }
    dates_tenors[posm] = sett_dates[pos_dates]
  }
  if (length(posy) != 0) {
    months_v = as.numeric(gsub("Y", "", tenors[posy], fixed = T)) * 12
    dates_month = add_months(sett_ini, months_v)
    pos_dates = findInterval(dates_month, sett_dates)
    nomatch = is.na(match(dates_month, sett_dates))
    if (conv == "F") {
      pos_dates[nomatch] = pos_dates[nomatch] + 1
    }else if (conv == "MF") {
      delta_month = as.numeric(format(dates_month[nomatch], "%m")) - as.numeric(format(sett_dates[pos_dates[nomatch] + 1], "%m"))
      pos_dates[nomatch] = pos_dates[nomatch] + (delta_month == 0)
    }
    dates_tenors[posy] = sett_dates[pos_dates]
  }
  dayst0 = as.numeric(dates_tenors - date_ini)
  dayst2 = as.numeric(dates_tenors - sett_ini)
  if((length(poson)+length(poson))>0){
    dayst2 = c(dayst0[c(poson, posw)], as.numeric(dayst2[-c(poson, posw)]))
  }
  return(list(dates_tenors = dates_tenors, dayst0 = dayst0, dayst2 = dayst2))
}
