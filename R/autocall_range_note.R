autocall_range_note<-function(ini_date, mat_date, fix_index_date, index, index_spot, index_ini,bond_cc_curve, 
                              dom_cc_curve, for_cc_curve, vol_matrix, base, coupon, cf_mat, ki_barrier, cpn_barrier, er_barrier, er_val_dates,
                              credit_spread=0, haz_curve=NULL, quote_delta=TRUE, quote_bfrr=TRUE, delta_type=1, rate_type="nominal", 
                              nominal, denom=100, round_digits=3, rho_mat=NULL, index_series=NULL, M=1e5, settDays, conv="act_360", vol_factor=1, quantil_pr_sim=0.1, rec=0.4){

  model_diff = switch(conv, "act_360" = diff_act_360, "act_365" = diff_act_365, 
                      "act_act" = diff_act_act, "30_360" = diff_30_360, "30_360I" = diff_30_360I, 
                      "30_360E" = diff_30_360E, nl_365 = diff_nl_365)
  
  dmat=as.numeric(mat_date-ini_date)
  pr_cash=pr_perc=pr_denom=index_vol=0
  if(dmat>=0){
  nind=length(index)
  dind=as.numeric(fix_index_date-ini_date)
  r=unlist(lapply(dom_cc_curve,function(curve){approx_extrap(x=curve[,1],y=curve[,2], xout=dind)$y}))
  rf=unlist(lapply(for_cc_curve,function(curve){approx_extrap(x=curve[,1],y=curve[,2], xout=dind)$y}))
  tn=ifelse(dind<0,0,dind)/base
  names(index_ini)=index
  
  vol=sapply(index,function(x){vol_extract(d=dind, strike=index_ini[[x]], spot=index_spot[[x]], r=r[[x]], rf=rf[[x]], base=base, vol_matrix=vol_matrix[[x]], quote_delta=quote_delta, quote_bfrr=quote_bfrr, delta_type=delta_type, rate_type=rate_type)})
  vol=vol*vol_factor
  if(is.null(haz_curve)){sd_prob=1}else{sd_prob=sd_prob(dmat, haz_curve)$sp_acum}
  
  r_c = r
  rf_c= rf
  if (substr(rate_type, 1, 1) == "n") {
    r_c = log(1 + r * tn)/tn
    rf_c = log(1 + rf * tn)/tn
  }
  if (substr(rate_type, 1, 1) == "e") {
    r_c = log(1 + r)
    rf_c = log(1 + rf)
  }
  min_date=max(cf_mat$INI_DATE)
  max_date=max(cf_mat$FIN_DATE)
  days_cf=as.numeric(max(cf_mat$PAY_DATE)-ini_date)
  N_days=as.numeric(max_date-min_date)
  pos_cf=which(cf_mat$FIN_DATE>ini_date)
  lcpn=1
  t_cpn=tail(model_diff(cf_mat$FIN_DATE)$times,lcpn)
  
  if(length(pos_cf)>0){
    min_date=min(cf_mat$INI_DATE[pos_cf])
    max_date=max(cf_mat$FIN_DATE[pos_cf])
    days_cf=as.numeric(cf_mat$PAY_DATE[pos_cf]-ini_date)
    dates_seq=settDays[findInterval(min_date,settDays):findInterval(max_date,settDays)]
    dates_fut=dates_seq[-c(1:findInterval(ini_date,dates_seq))]
    pos_cpn=match(cf_mat$FIN_DATE[pos_cf],dates_fut)
    N_days=as.numeric(findInterval(cf_mat$FIN_DATE[pos_cf],settDays)-findInterval(cf_mat$INI_DATE[pos_cf], settDays))  
    lcpn=length(pos_cpn)
    t_cpn=tail(model_diff(cf_mat$FIN_DATE)$times,lcpn)
  }
  dates_seq=settDays[findInterval(min_date,settDays):findInterval(max_date,settDays)]
  dates_past=dates_seq[1:findInterval(ini_date,dates_seq)]
  dates_fut=dates_seq[-c(1:findInterval(ini_date,dates_seq))]
  
  index_past=index_series[findInterval(dates_past,index_series[,1]),-1]
  n_in_acum=sum(apply(index_past,1,function(x)all(x>=cpn_barrier)))
  
  t_seq=as.numeric(dates_fut-ini_date)/base
  drift=r_c-rf_c
  rho_index=rho_mat[match(index,colnames(rho_mat)),match(index,colnames(rho_mat))]
  er_dates_fut=er_val_dates[er_dates>ini_date]
  pos_er=NULL
  if(!is.na(er_dates_fut)){pos_er=findInterval(er_dates_fut,dates_fut)}
  

  st_simul=spot_simul_mult_barrier(spot=index_spot, spot_ini=index_ini, times=t_seq, rates=drift, sigma=vol, rho=rho_index, M=M, cpn_barrier=cpn_barrier, er_barrier=er_barrier, ki_barrier=ki_barrier, pos_er=pos_er) 
  rb=approx_extrap(x=bond_cc_curve[,1],y=bond_cc_curve[,2], xout=days_cf)$y
  
  df=cc_price(days=dmat,rates=rb+credit_spread, base=base,rate_type=rate_type)*(sd_prob+rec*(1-sd_prob))
  cc_rate_pay=disc_rate(df, days=dmat, base=base, rate_type=rate_type)
  
  
  
coupon=coupon*t_cpn
  if(lcpn==1){
    if(length(t_seq)==0){
      days_cpn_sim=st_simul$cpn_barrier_ind+n_in_acum
    }
    if(length(t_seq)>0){
    if(dim(st_simul$cpn_barrier_ind)[1]==1){
    days_cpn_sim=apply(st_simul$cpn_barrier_ind,2,cumsum)[pos_cpn]+n_in_acum
    }else{
    days_cpn_sim=apply(st_simul$cpn_barrier_ind,2,cumsum)[pos_cpn,]+n_in_acum  
    }
    }
    delta_cpn_sim=days_cpn_sim/N_days
    ki_lprs=st_simul$ki_lprs
    cf_sim=(coupon*delta_cpn_sim+st_simul$ki_barrier_ind+st_simul$ki_lprs*!st_simul$ki_barrier_ind)
    pr_perc=100*df*mean(cf_sim)
    bond_pr_perc=df*quantile(cf_sim,quantil_pr_sim)
  }
  
  
  if(lcpn>1){
    days_cpn_sim_acum=apply(st_simul$cpn_barrier_ind,2,cumsum)[pos_cpn,]
    days_cpn_sim=rbind(days_cpn_sim_acum[1,]+n_in_acum, apply(days_cpn_sim_acum,2,diff))
    delta_cpn_sim=days_cpn_sim/N_days
    cf_sim=coupon*delta_cpn_sim
    
    ki_lprs=st_simul$ki_lprs
    
    cf_sim[lcpn,]=cf_sim[lcpn,]+st_simul$ki_barrier_ind+st_simul$ki_lprs*!st_simul$ki_barrier_ind
    pos_cpn_er=match(pos_er,pos_cpn)
    if(length(pos_cpn_er)==1){
      cf_sim[pos_cpn_er,st_simul$er_barrier_ind]=cf_sim[pos_cpn_er,st_simul$er_barrier_ind]+1
      cf_sim[(pos_cpn_er+1):lcpn,st_simul$er_barrier_ind]=0  
    }
    if(length(pos_cpn_er)>1){
      j=1
    for(x in pos_cpn_er){
      cf_sim[x,st_simul$er_barrier_ind[j,]]=cf_sim[x,st_simul$er_barrier_ind[j,]]+1
      cf_sim[(x+1):lcpn,st_simul$er_barrier_ind[j,]]=0
      j=j+1
    }
    }
    disc_cf_sim=cf_sim*df
    disc_pr_sim=apply(disc_cf_sim,2,sum)
    bond_pr_perc=quantile(disc_pr_sim,quantil_pr_sim)
    pr_perc=100*mean(disc_pr_sim)
  }
    
  
  pr_cash=nominal*pr_perc/100
  pr_denom=denom*pr_perc/100
  bond_pr_denom=bond_pr_perc*denom
  deriv_pr_denom=pr_denom-bond_pr_denom
  }
  return(list(pr_cash_pay=round(pr_cash, round_digits), pr_perc=round(pr_perc, round_digits), pr_denom=round(pr_denom, round_digits), 
              bond_pr_denom = round(bond_pr_denom, round_digits), deriv_pr_denom = round(deriv_pr_denom, round_digits), index_vol=round(vol, round_digits), disc_rates=round(cc_rate_pay, round_digits), days_mat=dmat, 
              index_spot=round(index_spot, round_digits),r_dom=round(r, round_digits),r_for=round(rf, round_digits)))
}
