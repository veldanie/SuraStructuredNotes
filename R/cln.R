cln<-function(ini_date, coupon, freq, mat_date, haz_curve, ref_curr, nom_curr, nom=1, rec=0.4, conv="act_360", fx_spot=1, fwd_points=0, factor_fwd=1, serie_float_rate = NULL, disc_curve=NULL, base_disc=360, proj_curve= NULL, base_proj=360, denom=100, spread=0, round_digits=5, base_currs=c("NZD","AUD","EUR","GBP"), ytm_interval=c(0,1)){
     

  dmat = as.numeric(mat_date - ini_date)
  cln_pr_perc=cln_pr_curr=cln_pr_denom=bond_pr_denom=deriv_pr_denom=0
  if (dmat >= 0) {
  
  model_count = switch(conv, act_360 = days_act_360, act_365 = days_act_365, 
                       act_act = days_act_act, `30_360` = days_30_360, `30_360I` = days_30_360I, 
                       `30_360E` = days_30_360E, nl_365 = days_nl_365)
  model_diff = switch(conv, act_360 = diff_act_360, act_365 = diff_act_365, 
                      act_act = diff_act_act, `30_360` = diff_30_360, `30_360I` = diff_30_360I, 
                      `30_360E` = diff_30_360E, nl_365 = diff_nl_365)
 
  per = 1
  length = 1
  if (freq != 0) {
    per = 12/freq
    length = as.numeric(freq * (mat_date - ini_date)/365) + 2
  }
  if (freq == 0) {
    freq = 1
  }
  cpn_dates = sort(seq(mat_date, by = paste0("-", per, " months"), length = length))
  fut_cpn_dates = cpn_dates[cpn_dates > ini_date]
  prev_date = tail(cpn_dates[cpn_dates < ini_date], 1)
  lfutcpn = length(fut_cpn_dates)
  times_mod = model_count(fut_cpn_dates, ini_date)
  days=times_mod$days
  times=times_mod$times
  delta = tail(model_diff(cpn_dates)$times, length(fut_cpn_dates))
  if (is.na(delta) & coupon != 0) {
    delta = model_count(fut_cpn_dates, issue_date)$times
  }
  delta = ifelse(is.na(delta), 1, delta)
  
  if (!is.null(serie_float_rate)) {
    spr=coupon
    float_rate_0 = serie_float_rate[which(serie_float_rate[,1] == prev_date), 2]
    proj_rates = approx_extrap(x = proj_curve[, 1], y = proj_curve[, 2], xout = days)$y
    delta_fwdrate = delta
    proj_fwd = ((1 + proj_rates[-1] * days[-1]/base_proj)/(1 + proj_rates[-lfutcpn] * days[-lfutcpn]/base_proj) - 1) * 1/delta[-1]
    proj_fwd = c(float_rate_0, proj_fwd)
    coupon=proj_fwd+spr
  }

  cpn_acum=coupon[1]*model_count(ini_date,prev_date)$times
  cf = coupon * delta
  
  disc_rates=approx_extrap(x = disc_curve[, 1], y = disc_curve[, 2], xout = days)$y
  probs=sd_prob(days, haz_curve, base_disc)
  
  if(ref_curr==nom_curr){
    disc_rates_curr=disc_rates
  }
  if(ref_curr!=nom_curr & !any(nom_curr == base_currs) & (nom_curr!="USD"|any(ref_curr == base_currs))){
    pips=fwd_points[days]/factor_fwd
    disc_rates_curr = ((1 + disc_rates * days/base_disc) * (pips/fx_spot + 1) - 1) * base_disc/days    
  }
  if (ref_curr!=nom_curr & any(nom_curr == base_currs)){
    pips=fwd_points[days]/factor_fwd
    disc_rates_curr = ((1 + disc_rates * days/base_disc)/(pips/fx_spot + 1) - 1) * base_disc/days
  }
  
  df_rf=1/(1+(disc_rates_curr)*days/base_disc)
  df=1/(1+(disc_rates_curr+spread)*days/base_disc)
  
  cln_pr_perc=round((sum(cf*df*probs$sp)+tail(probs$sp,1)*tail(df,1)-(1-rec)*sum(probs$dp*df))*100,round_digits)
  cln_pr_denom=cln_pr_perc*denom/100
  cln_pr_curr=cln_pr_perc*nom/100
  bond_pr_denom=round(denom*(sum(cf*df_rf)+tail(df,1)),round_digits)
  deriv_pr_denom=cln_pr_denom-bond_pr_denom
  
  tdf=df*days/base_disc
  
  mac_dur = round(-(sum(cf * tdf)+tail(tdf,1))/(cln_pr_perc/100), round_digits)
  ytm_approx=uniroot(function(ytm,cln_pr_perc,days, base_disc){cln_pr_perc-100*sum(cf/(1+ytm*days/base_disc))-100/(1+ytm*tail(days,1)/base_disc)}, interval=ytm_interval, cln_pr_perc,days,base_disc)$root
  mod_dur = round(mac_dur/(1 + ytm_approx/freq), round_digits)
  
  tdf2=(times/freq + times^2) * cf * df
  convex = round((sum(tdf2)+(tail(times,1)/freq+tail(times,1)^2)*tail(df,1))/((cln_pr_perc/100) * (1 + ytm_approx/freq)^2), round_digits)
  
  clean_cln_pr_perc=cln_pr_perc-cpn_acum*100
  clean_cln_pr_denom=clean_cln_pr_perc*denom/100
  }
  return(list(pr_perc=cln_pr_perc,pr_curr=cln_pr_curr,pr_denom=cln_pr_denom, clean_pr_denom=clean_cln_pr_denom, clean_pr_perc=clean_cln_pr_perc, days_mat=dmat, bond_pr_denom=bond_pr_denom, deriv_pr_denom=deriv_pr_denom, rf_rates_curr=disc_rates_curr, ytm_approx=ytm_approx*100, mac_dur=mac_dur, mod_dur=mod_dur, convex=convex))
}
