swap_price<-function (val_date, cf_dates_leg1, nom_leg1, disc_curveT0_leg1,
                      rate_leg1 = 0, proj_curve_leg1 = NA, liq_proj_leg1 = 0, liq_rate_leg1 = 0, efec_rate_leg1 = 0,
                      amort_leg1 = rep(0, length(cf_dates_leg1)), base_leg1 = 360,
                      conv_cf_leg1 = "act_360", conv_fwdrate_leg1 = "act_360", pay_receive_leg1 = 1, curr1_cop = 1,
                      type_leg1 = NA, index_leg1 = NA, cf_dates_leg2, nom_leg2,
                      disc_curveT0_leg2, rate_leg2 = 0, proj_curve_leg2 = NA, liq_proj_leg2 = 0, liq_rate_leg2 = 0,
                      efec_rate_leg2 = 0, amort_leg2 = rep(0, length(cf_dates_leg2)),
                      base_leg2 = 360, conv_cf_leg2 = "act_360", conv_fwdrate_leg2 = "act_360", pay_receive_leg2 = -1,
                      curr2_cop = 1, type_leg2 = NA, index_leg2 = NA, sett_dates, diff_days_fix_value_dates=0, adj_non_sett_dates=FALSE)
{
  if (length(cf_dates_leg1) != length(amort_leg1)) {
    stop("Longitud de vector de amortizaciones debe coincidir con numero de fechas")
  }
  pay_date_leg1 = length(which(val_date == cf_dates_leg1))
  ldates_leg1 = length(cf_dates_leg1)
  fut_dates_leg1 = cf_dates_leg1[cf_dates_leg1 > val_date]
  prev_dates_leg1 = cf_dates_leg1[cf_dates_leg1 <= val_date]
  lcf_leg1 = length(fut_dates_leg1)
  prev_date_leg1 = tail(cf_dates_leg1, lcf_leg1 + 1)[1]

  model_diff1 = switch(conv_cf_leg1, act_360 = diff_act_360,
                       act_365 = diff_act_365, `30_360` = diff_30_360,`30_365` = diff_30_365, `30_360I` = diff_30_360I,
                       `30_360E` = diff_30_360E, nl_365 = diff_nl_365, nl_360 = diff_nl_360)
  base_cf_leg1 = switch(conv_cf_leg1, act_360 = 360, act_365 = 365,
                        `30_360` = 360, `30_365` = 365, `30_360I` = 360, `30_360E` = 360, nl_365 = 365, nl_360 = 360)

  delta_all_cf_leg1 = model_diff1(cf_dates_leg1)$times
  delta_cf_leg1 = tail(delta_all_cf_leg1, lcf_leg1)

  fut_amort_leg1 = tail(amort_leg1, lcf_leg1)
  days_leg1 = as.numeric(fut_dates_leg1 - val_date)
  adjust_days_leg1=days_leg1;adjust_fut_dates_leg1=fut_dates_leg1
  if(!is.na(diff_days_fix_value_dates) & diff_days_fix_value_dates>0){
    adjust_fut_dates_leg1=adjust_dates(dates=fut_dates_leg1,sett_dates=sett_dates,n_sett_days=diff_days_fix_value_dates)
    adjust_days_leg1 = as.numeric(adjust_fut_dates_leg1 - val_date)
  }
  if((diff_days_fix_value_dates==0 | is.na(diff_days_fix_value_dates)) & adj_non_sett_dates==TRUE){
    adjust_fut_dates_leg1=adjust_dates(adjust_fut_dates_leg1, sett_dates, 0)
    adjust_days_leg1 = as.numeric(adjust_fut_dates_leg1 - val_date)
  }
  nom_net_leg1 = nom_leg1 - c(0, amort_leg1[-ldates_leg1])
  fut_nom_net_leg1 = tail(nom_net_leg1, lcf_leg1)
  prev_rate_leg1 = next_rate_leg1 = rate_leg1
  proj_fwd_leg1 = 0
  if (all(type_leg1 != c("OIS","ois","Ois","IPC","ipc","Ipc")) & all(!is.na(index_leg1))) {
    prev_fixdate_leg1=sett_dates[findInterval(prev_date_leg1,sett_dates)-liq_rate_leg1]
    next_rate_leg1 = index_leg1[match(prev_fixdate_leg1, index_leg1[, 1]), 2]
  }
  if (any(type_leg1 == c("OIS", "ois", "Ois")) & all(!is.na(index_leg1))) {
    index_dates = index_leg1[, 1]
    index_val = index_leg1[, 2]
    pos_inf = which(prev_date_leg1 == index_dates)
    pos_sup = min(union(which(val_date < index_dates), which(fut_dates_leg1[1] == index_dates)))
    days_acum = as.numeric(index_dates[pos_sup] - prev_date_leg1)
    days_fwd = as.numeric(fut_dates_leg1[1] - index_dates[pos_sup])
    liq_days = as.numeric(index_dates[pos_sup] - val_date)
    ois_acum = round((index_val[pos_sup]/index_val[pos_inf] - 1) * base_leg1/days_acum, 5)
    ois_rates = approx(x=proj_curve_leg1[,1], y=proj_curve_leg1[,2],xout=c(liq_days, days_fwd + liq_days))$y
    if (days_fwd == 0) {
      ois_fwd = 0
    }else{
      ois_fwd = round(((1 + ois_rates[2] * (days_fwd + liq_days)/base_leg1)/(1 + ois_rates[1] * liq_days/base_leg1) - 1) * base_leg1/days_fwd, 5)
    }
    next_rate_leg1 = ((1 + ois_acum * days_acum/base_leg1) * (1 + ois_fwd * days_fwd/base_leg1) - 1) * base_leg1/(days_fwd + days_acum)
  }

  if (any(type_leg1 == c("IPC", "ipc", "Ipc")) & all(!is.na(index_leg1))) {
    index_dates = index_leg1[, 1]
    index_val = index_leg1[, 2]
    prev_date_leg1_ipc=seq(from=fut_dates_leg1[1], by="-12 months",length=2)[2]
    pos_inf = findInterval(prev_date_leg1_ipc, index_dates)
    pos_sup = findInterval(val_date, index_dates)
    ipc_proj=1
    if(format(val_date, "%m%Y")!=format(fut_dates_leg1[1], "%m%Y")){
      ipc_proj=(1+approx(x=proj_curve_leg1[,1],y=proj_curve_leg1[,2], xout=days_leg1[1])$y*days_leg1[1]/base_leg1)
    }
    next_rate_leg1=round((index_val[pos_sup]/index_val[pos_inf])*ipc_proj-1,4)
  }

  if (ldates_leg1 >= (lcf_leg1 + 2) & pay_date_leg1 == 1) {
    prev_dates = tail(prev_dates_leg1, 2)
    prev_delta_leg1 = tail(delta_all_cf_leg1, lcf_leg1 +1)[1]
    prev_amort_leg1 = tail(amort_leg1, lcf_leg1 + 1)[1]
    prev_nom_leg1 = tail(nom_net_leg1, lcf_leg1 + 1)[1]
    if (any(type_leg1 == c("OIS", "ois", "Ois")) & !is.na(index_leg1)) {
      pos_inf = which(prev_dates[1] == index_dates)
      pos_sup = which(prev_dates[2] == index_dates)
      prev_rate_leg1 = round((index_val[pos_sup]/index_val[pos_inf] - 1) * 1/prev_delta_leg1, 5)
    }
    if (all(type_leg1 != c("OIS","ois","Ois","IPC","ipc","Ipc")) & all(!is.na(index_leg1))) {
      prev_fixdate_leg1=sett_dates[findInterval(prev_dates[1],sett_dates)-liq_rate_leg1]
      prev_rate_leg1 = index_leg1[which(prev_fixdate_leg1 == index_leg1[, 1]), 2]
    }
    if (any(type_leg1 == c("IPC", "ipc", "Ipc")) & all(!is.na(index_leg1))) {
      index_dates = index_leg1[, 1]
      index_val = index_leg1[, 2]
      prev_date_leg1_ipc=seq(from=fut_dates_leg1[1], by="-12 months",length=2)[2]
      pos_inf = findInterval(prev_date_leg1_ipc, index_dates)
      pos_sup = findInterval(val_date, index_dates)
      prev_rate_leg1=round((index_val[pos_sup]/index_val[pos_inf])*ipc_proj-1,4)
    }
    prev_cf_leg1 = pay_receive_leg1 * (prev_nom_leg1 * ((prev_rate_leg1 * prod(!is.na(proj_curve_leg1)) + rate_leg1) * prev_delta_leg1) + prev_amort_leg1)
    prev_cf_leg1_cop = prev_cf_leg1 * curr1_cop
  }else{
    prev_cf_leg1_cop = 0
  }
  if (!is.na(proj_curve_leg1)) {

    liq_proj_leg1_real=liq_proj_leg1
    if(liq_proj_leg1>0){liq_proj_leg1_real=sett_dates[findInterval(val_date, sett_dates)+liq_proj_leg1]-val_date}
    days_proj_leg1 = days_leg1 - liq_proj_leg1_real
    days_proj_leg1[days_proj_leg1<=0]=1
    proj_rates_leg1 = approx_extrap(x = proj_curve_leg1[, 1], y = proj_curve_leg1[, 2], xout = days_proj_leg1)$y
    delta_fwdrate_leg1=delta_cf_leg1
    if(conv_fwdrate_leg1!=conv_cf_leg1){
      model_diff_fwdrate1 = switch(conv_fwdrate_leg1, act_360 = diff_act_360,
                                   act_365 = diff_act_365, `30_360` = diff_30_360, `30_365` = diff_30_365, `30_360I` = diff_30_360I,
                                   `30_360E` = diff_30_360E, nl_365 = diff_nl_365, nl_360 = diff_nl_360)
      delta_all_fwdrate_leg1 = model_diff_fwdrate1(cf_dates_leg1)$times
      delta_fwdrate_leg1 = tail(delta_all_fwdrate_leg1, lcf_leg1)
    }
    proj_fwd_leg1 = ((1 + proj_rates_leg1[-1] * days_proj_leg1[-1]/base_leg1)/(1 + proj_rates_leg1[-lcf_leg1] * days_proj_leg1[-lcf_leg1]/base_leg1) - 1) * 1/delta_fwdrate_leg1[-1]
    proj_fwd_leg1 = c(next_rate_leg1, proj_fwd_leg1)
  }
  disc_rates_leg1 = approx_extrap(x=disc_curveT0_leg1[,1],y=disc_curveT0_leg1[,2],xout=adjust_days_leg1)$y
  df_leg1 = 1/(1 + disc_rates_leg1 * adjust_days_leg1/base_leg1)
  if (efec_rate_leg1 == 1) {
    proj_fwd_efec_leg1 = c(proj_fwd_leg1[1], efec_rate(proj_fwd_leg1[-1], round(delta_fwdrate_leg1[-1] * base_cf_leg1), base_leg1, base_cf_leg1))
    proj_rate_leg1=(1 + proj_fwd_efec_leg1) * (1 + rate_leg1)-1
    cf_int_leg1 = (fut_nom_net_leg1) * (((1 + proj_fwd_efec_leg1) * (1 + rate_leg1))^delta_cf_leg1 - 1)
    cf_leg1=cf_int_leg1+fut_amort_leg1
  }else{
    proj_rate_leg1=proj_fwd_leg1 + rate_leg1
    cf_int_leg1 = fut_nom_net_leg1 * proj_rate_leg1 * delta_cf_leg1
    cf_leg1 = cf_int_leg1+fut_amort_leg1
  }
  pv_leg1 = sum(cf_leg1 * df_leg1) * pay_receive_leg1
  pv_leg1_cop = pv_leg1 * curr1_cop
  pay_date_leg2 = length(which(val_date == cf_dates_leg2))
  ldates_leg2 = length(cf_dates_leg2)
  fut_dates_leg2 = cf_dates_leg2[cf_dates_leg2 > val_date]
  prev_dates_leg2 = cf_dates_leg2[cf_dates_leg2 <= val_date]
  lcf_leg2 = length(fut_dates_leg2)
  prev_date_leg2 = tail(cf_dates_leg2, lcf_leg2 + 1)[1]

  model_diff2 = switch(conv_cf_leg2, act_360 = diff_act_360,
                       act_365 = diff_act_365, act_act = diff_act_act, `30_360` = diff_30_360,`30_365` = diff_30_365,
                       `30_360I` = diff_30_360I, `30_360E` = diff_30_360E, nl_365 = diff_nl_365, nl_360 = diff_nl_360)
  base_cf_leg2 = switch(conv_cf_leg2, act_360 = 360, act_365 = 365,
                        `30_360` = 360, `30_365` = 365, `30_360I` = 360, `30_360E` = 360, nl_365 = 365, nl_360 = 360)
  delta_all_cf_leg2 = model_diff2(cf_dates_leg2)$times
  delta_cf_leg2 = tail(delta_all_cf_leg2, lcf_leg2)
  fut_amort_leg2 = tail(amort_leg2, lcf_leg2)
  days_leg2 = as.numeric(fut_dates_leg2 - val_date)
  adjust_days_leg2=days_leg2;adjust_fut_dates_leg2=fut_dates_leg2
  if(!is.na(diff_days_fix_value_dates) & diff_days_fix_value_dates>0){
    adjust_fut_dates_leg2=adjust_dates(dates=fut_dates_leg2,sett_dates=sett_dates,n_sett_days=diff_days_fix_value_dates)
    adjust_days_leg2 = as.numeric(adjust_fut_dates_leg2 - val_date)
  }
  if((diff_days_fix_value_dates==0 | is.na(diff_days_fix_value_dates)) & adj_non_sett_dates==TRUE){
    adjust_fut_dates_leg2=adjust_dates(adjust_fut_dates_leg2, sett_dates, 0)
    adjust_days_leg2 = as.numeric(adjust_fut_dates_leg2 - val_date)
  }

  nom_net_leg2 = nom_leg2 - c(0, amort_leg2[-ldates_leg2])
  fut_nom_net_leg2 = tail(nom_net_leg2, lcf_leg2)
  prev_rate_leg2 = next_rate_leg2 = rate_leg2
  proj_fwd_leg2 = 0
  if (all(type_leg2 != c("OIS","ois","Ois", "IPC", "Ipc","ipc")) & all(!is.na(index_leg2))) {
    prev_fixdate_leg2=sett_dates[findInterval(prev_date_leg2,sett_dates)-liq_rate_leg2]
    next_rate_leg2 = index_leg2[findInterval(prev_fixdate_leg2, index_leg2[, 1]), 2]
  }
  if (any(type_leg2 == c("OIS", "ois", "Ois")) & all(!is.na(index_leg2))) {
    index_dates = index_leg2[, 1]
    index_val = index_leg2[, 2]
    pos_inf = which(prev_date_leg2 == index_dates)
    pos_sup = min(union(which(val_date < index_dates), which(fut_dates_leg2[1] ==  index_dates)))
    days_acum = as.numeric(index_dates[pos_sup] - prev_date_leg2)
    days_fwd = as.numeric(fut_dates_leg2[1] - index_dates[pos_sup])
    liq_days = as.numeric(index_dates[pos_sup] - val_date)
    ois_acum = round((index_val[pos_sup]/index_val[pos_inf] - 1) * base_leg2/days_acum, 5)
    ois_rates = approx(x=proj_curve_leg2[,1], y=proj_curve_leg2[,2],xout=c(liq_days, days_fwd + liq_days))$y

    if (days_fwd == 0) {
      ois_fwd = 0
    }else{
      ois_fwd = round(((1 + ois_rates[2] * (days_fwd + liq_days)/base_leg2)/(1 + ois_rates[1] * liq_days/base_leg2) - 1) * base_leg2/days_fwd, 5)
    }
    next_rate_leg2 = ((1 + ois_acum * days_acum/base_leg2) * (1 + ois_fwd * days_fwd/base_leg2) - 1) * base_leg2/(days_fwd + days_acum)
  }
  if (any(type_leg2 == c("IPC", "ipc", "Ipc")) & all(!is.na(index_leg2))) {
    index_dates = index_leg2[, 1]
    index_val = index_leg2[, 2]
    prev_date_leg2_ipc=seq(from=fut_dates_leg2[1], by="-12 months",length=2)[2]
    pos_inf = findInterval(prev_date_leg2_ipc, index_dates)
    pos_sup = findInterval(val_date, index_dates)
    ipc_proj=1
    if(format(val_date, "%m%Y")!=format(fut_dates_leg2[1], "%m%Y")){
      ipc_proj=(1+approx(x=proj_curve_leg2[,1],y=proj_curve_leg2[,2], xout=days_leg2[1])$y*days_leg2[1]/base_leg2)
    }
    next_rate_leg2=round((index_val[pos_sup]/index_val[pos_inf])*ipc_proj-1,4)
  }

  if (ldates_leg2 >= (lcf_leg2 + 2) & pay_date_leg2 == 1) {
    prev_dates = tail(prev_dates_leg2, 2)
    prev_delta_leg2 = tail(delta_all_cf_leg2, lcf_leg2 + 1)[1]
    prev_amort_leg2 = tail(amort_leg2, lcf_leg2 + 1)[1]
    prev_nom_leg2 = tail(nom_net_leg2, lcf_leg2 + 1)[1]
    if (any(type_leg2 == c("OIS", "ois", "Ois")) & !is.na(index_leg2)) {
      pos_inf = which(prev_dates[1] == index_dates)
      pos_sup = which(prev_dates[2] == index_dates)
      prev_rate_leg2 = round((index_val[pos_sup]/index_val[pos_inf] -  1) * 1/prev_delta_leg2, 5)
    }
    if (all(type_leg2 != c("OIS","ois","Ois","IPC","ipc","Ipc")) & all(!is.na(index_leg2))) {
      prev_fixdate_leg2=sett_dates[findInterval(prev_dates[1],sett_dates)-liq_rate_leg2]
      prev_rate_leg2 = index_leg2[which(prev_fixdate_leg2 == index_leg2[, 1]), 2]
    }
    if (any(type_leg2 == c("IPC", "ipc", "Ipc")) & all(!is.na(index_leg2))) {
      index_dates = index_leg2[, 1]
      index_val = index_leg2[, 2]
      prev_date_leg2_ipc=seq(from=fut_dates_leg2[1], by="-12 months",length=2)[2]
      pos_inf = findInterval(prev_date_leg2_ipc, index_dates)
      pos_sup = findInterval(val_date, index_dates)
      prev_rate_leg2=round((index_val[pos_sup]/index_val[pos_inf])-1,4)
    }
    prev_cf_leg2 = pay_receive_leg2 * (prev_nom_leg2 * ((prev_rate_leg2 * prod(!is.na(proj_curve_leg2)) + rate_leg2) * prev_delta_leg2) + prev_amort_leg2)
    prev_cf_leg2_cop = prev_cf_leg2 * curr2_cop
  }else{
    prev_cf_leg2_cop = 0
  }
  if (!is.na(proj_curve_leg2)) {
    liq_proj_leg2_real=liq_proj_leg2
    if(liq_proj_leg2>0){liq_proj_leg2_real=sett_dates[findInterval(val_date, sett_dates)+liq_proj_leg2]-val_date}
    days_proj_leg2 = days_leg2 - liq_proj_leg2_real
    days_proj_leg2[days_proj_leg2<=0]=1
    proj_rates_leg2 = approx_extrap(x = proj_curve_leg2[, 1], y = proj_curve_leg2[, 2], xout = days_proj_leg2)$y
    delta_fwdrate_leg2=delta_cf_leg2
    if(conv_fwdrate_leg2!=conv_cf_leg2){
      model_diff_fwdrate2 = switch(conv_fwdrate_leg2, act_360 = diff_act_360,
                                   act_365 = diff_act_365, `30_360` = diff_30_360,`30_365` = diff_30_365, `30_360I` = diff_30_360I,
                                   `30_360E` = diff_30_360E, nl_365 = diff_nl_365, nl_360 = diff_nl_360)
      delta_all_fwdrate_leg2 = model_diff_fwdrate2(cf_dates_leg2)$times
      delta_fwdrate_leg2 = tail(delta_all_fwdrate_leg2, lcf_leg2)
    }
    proj_fwd_leg2 = ((1 + proj_rates_leg2[-1] * days_proj_leg2[-1]/base_leg2)/(1 + proj_rates_leg2[-lcf_leg2] * days_proj_leg2[-lcf_leg2]/base_leg2) - 1) * 1/delta_fwdrate_leg2[-1]
    proj_fwd_leg2 = c(next_rate_leg2, proj_fwd_leg2)
  }
  disc_rates_leg2 = approx_extrap(x=disc_curveT0_leg2[,1],y=disc_curveT0_leg2[,2],xout=adjust_days_leg2)$y
  df_leg2 = 1/(1 + disc_rates_leg2 * adjust_days_leg2/base_leg2)
  if (efec_rate_leg2 == 1) {
    proj_fwd_efec_leg2 = c(proj_fwd_leg2[1], efec_rate(proj_fwd_leg2[-1], round(delta_fwdrate_leg2[-1] * base_cf_leg2), base_leg2, base_cf_leg2))
    proj_rate_leg2=(1 + proj_fwd_efec_leg2) * (1 + rate_leg2)-1
    cf_int_leg2 = (fut_nom_net_leg2) * (((1 + proj_fwd_efec_leg2) * (1 + rate_leg2))^delta_cf_leg2 - 1)
    cf_leg2 = cf_int_leg2 + fut_amort_leg2

  }else {
    proj_rate_leg2=proj_fwd_leg2 + rate_leg2
    cf_int_leg2 = fut_nom_net_leg2 * proj_rate_leg2 * delta_cf_leg2
    cf_leg2 = cf_int_leg2 + fut_amort_leg2
  }

  pv_leg2 = sum(cf_leg2 * df_leg2) * pay_receive_leg2
  pv_leg2_cop = pv_leg2 * curr2_cop
  pv_pay = pv_leg1 * (pay_receive_leg1 < 0) + pv_leg2 * (pay_receive_leg2 < 0)
  pv_pay_cop = pv_leg1_cop * (pay_receive_leg1 < 0) + pv_leg2_cop * (pay_receive_leg2 < 0)
  pv_rec = pv_leg1 * (pay_receive_leg1 > 0) + pv_leg2 * (pay_receive_leg2 > 0)
  pv_rec_cop = pv_leg1_cop * (pay_receive_leg1 > 0) + pv_leg2_cop * (pay_receive_leg2 > 0)
  pv_net_cop = pv_pay_cop + pv_rec_cop
  val_date_pay_cop = prev_cf_leg1_cop + prev_cf_leg2_cop
  if (pay_receive_leg1 < 0) {
    disc_rates_pay = disc_rates_leg1
    disc_rates_rec = disc_rates_leg2

    dates_pay = adjust_fut_dates_leg1
    dates_rec = adjust_fut_dates_leg2
    cf_pay_proy = cf_leg1 * pay_receive_leg1
    cf_pay_int_proy = cf_int_leg1 * pay_receive_leg1
    cf_pay_disc = cf_pay_proy * df_leg1
    cf_pay_int_disc = cf_pay_int_proy * df_leg1
    cf_rec_proy = cf_leg2 * pay_receive_leg2
    cf_rec_int_proy = cf_int_leg2 * pay_receive_leg2
    cf_rec_disc = cf_rec_proy * df_leg2
    cf_rec_int_disc = cf_rec_int_proy * df_leg2

    cf_pay_disc_cop = cf_pay_disc * curr1_cop
    cf_rec_disc_cop = cf_rec_disc * curr2_cop
    cf_pay_int_disc_cop = cf_pay_int_disc * curr1_cop
    cf_rec_int_disc_cop = cf_rec_int_disc * curr2_cop

  }
  if (pay_receive_leg1 > 0) {
    disc_rates_pay = disc_rates_leg2
    disc_rates_rec = disc_rates_leg1
    dates_pay = adjust_fut_dates_leg2
    dates_rec = adjust_fut_dates_leg1
    cf_pay_proy = cf_leg2 * pay_receive_leg2
    cf_pay_int_proy = cf_int_leg2 * pay_receive_leg2

    cf_pay_disc = cf_pay_proy * df_leg2
    cf_pay_int_disc = cf_pay_int_proy * df_leg2

    cf_rec_proy = cf_leg1 * pay_receive_leg1
    cf_rec_int_proy = cf_int_leg1 * pay_receive_leg1

    cf_rec_disc = cf_rec_proy * df_leg1
    cf_rec_int_disc = cf_rec_int_proy * df_leg1
    cf_rec_disc_cop = cf_rec_disc * curr1_cop
    cf_pay_disc_cop = cf_pay_disc * curr2_cop
    cf_rec_int_disc_cop = cf_rec_int_disc * curr1_cop
    cf_pay_int_disc_cop = cf_pay_int_disc * curr2_cop

  }
  return(list(pv_rec = pv_rec, pv_pay = pv_pay, pv_rec_cop = pv_rec_cop,
              pv_pay_cop = pv_pay_cop, pv_net_cop = pv_net_cop, val_date_pay_cop = val_date_pay_cop,
              cf_pay_proy = cf_pay_proy, cf_rec_proy = cf_rec_proy,
              cf_pay_disc = cf_pay_disc, cf_rec_disc = cf_rec_disc,
              dates_pay = dates_pay, dates_rec = dates_rec, cf_pay_disc_cop = cf_pay_disc_cop,
              cf_rec_disc_cop = cf_rec_disc_cop, cf_pay_int_proy=cf_pay_int_proy, cf_rec_int_proy=cf_rec_int_proy,
              cf_pay_int_disc_cop=cf_pay_int_disc_cop, cf_rec_int_disc_cop=cf_rec_int_disc_cop, disc_rates_pay=disc_rates_pay, disc_rates_rec=disc_rates_rec))
}
