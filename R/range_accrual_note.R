range_accrual_note<-function(ini_date, mat_date, fix_index_date, index_spot, bond_cc_curve, 
                              vol, base, coupon, cf_mat, lower_barrier, upper_barrier, 
                              credit_spread=0, haz_curve=NULL, rate_type="nominal", 
                              nominal, denom=100, round_digits=3, index_series=NULL, M=1e5, settDays, conv="act_360", rec=0.4){

  model_diff = switch(conv, "act_360" = diff_act_360, "act_365" = diff_act_365, 
                      "act_act" = diff_act_act, "30_360" = diff_30_360, "30_360I" = diff_30_360I, 
                      "30_360E" = diff_30_360E, nl_365 = diff_nl_365)
  
  dmat=as.numeric(mat_date-ini_date)
  pr_cash=pr_perc=pr_denom=index_vol=0
  if(dmat>=0){
  
  dind=as.numeric(fix_index_date-ini_date)
  tn=ifelse(dind<0,0,dind)/base
  
  if(is.null(haz_curve)){sd_prob=1}else{sd_prob=sd_prob(dmat, haz_curve)$sp_acum}
  
  pos_cf=which(cf_mat$FIN_DATE>ini_date)
  min_date=min(cf_mat$INI_DATE[pos_cf])
  max_date=max(cf_mat$FIN_DATE[pos_cf])
  dates_seq=settDays[findInterval(min_date,settDays):findInterval(max_date,settDays)]
  dates_past=dates_seq[1:findInterval(ini_date,dates_seq)]
  dates_fut=dates_seq[-c(1:findInterval(ini_date,dates_seq))]
  index_past=index_series[findInterval(dates_past,index_series[,1]),-1]
  
  n_in_acum=sum(index_past>=lower_barrier & index_past<=upper_barrier)
  
  t_seq=as.numeric(dates_fut-ini_date)/base
  
  st_simul=spot_simul(spot=index_spot, exp_times=t_seq, rates=0, sigma=vol, m=M) 
  
  days_cf=as.numeric(cf_mat$PAY_DATE[pos_cf]-fecha_ini)
  rb=approx_extrap(x=bond_cc_curve[,1],y=bond_cc_curve[,2], xout=days_cf)$y
  
  df=cc_price(days=dmat,rates=rb+credit_spread, base=base,rate_type=rate_type)*(sd_prob+rec*(1-sd_prob))
  cc_rate_pay=disc_rate(df, days=dmat, base=base, rate_type=rate_type)
  
  pos_cpn=findInterval(cf_mat$FIN_DATE[pos_cf],dates_fut)
  N_days=as.numeric(findInterval(cf_mat$FIN_DATE[pos_cf],settUS)-findInterval(cf_mat$INI_DATE[pos_cf], settUS))
  
  lcpn=length(pos_cpn)
  t_cpn=tail(model_diff(cf_mat$FIN_DATE)$times,lcpn)
  coupon_per=coupon*t_cpn
  
  days_cpn_sim_acum=apply((st_simul>=lower_barrier & st_simul<=upper_barrier),2,cumsum)[pos_cpn,]
  days_cpn_sim=rbind(days_cpn_sim_acum[1,]+n_in_acum, apply(days_cpn_sim_acum,2,diff))
  delta_cpn_sim=days_cpn_sim/N_days
  cf_sim=(df*coupon_per*delta_cpn_sim)
  pr_perc=100*(mean(apply(cf_sim,2,sum))+tail(df,1))
    
  
  pr_cash = round(nominal*pr_perc/100, round_digits)
  pr_denom = round(denom*pr_perc/100, round_digits)
  bond_pr_denom = round(tail(df,1)*denom, round_digits)
  deriv_pr_denom = round(pr_denom-bond_pr_denom, round_digits)
  }
  return(list(pr_perc = pr_perc, pr_curr = pr_cash, 
              pr_denom = pr_denom, days_mat = dmat, bond_pr_denom = bond_pr_denom, 
              deriv_pr_denom = deriv_pr_denom))
}
