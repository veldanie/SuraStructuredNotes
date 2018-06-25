basket_note<-function(ini_date, mat_date, fix_index_date, fix_spot_date, ini_date_avg, fin_date_avg, obs_dates=NULL, fx_spot, index, index_spot, index_ini, basket_ini=100, weight, a, b, max_ret, 
                            nom_pay, nom_prot, bond_cc_curve, dom_cc_curve, for_cc_curve, vol_matrix, base, coupon=0, credit_spread=0, 
                            fwd_points, haz_curve=NULL, quote_delta=TRUE, quote_bfrr=TRUE, delta_type=1, rate_type="nominal", 
                            denom=100, pay_curr="USD", nom_curr="COP", index_curr="USD", round_digits=3, quanto=1, quanto_id=NULL, quanto_spot=NULL,  
                            vol_matrix_quanto=NULL, quanto_cc_curve=NULL, rho_mat=NULL, index_series=NULL, float_strike=FALSE, geometric=FALSE, M=1e5, vol_factor=1, rec=0.4){

  ##a: Capital Protection (e.g. 0.95)
  ##b: Participation Rate (e.g. 0.5)
  ##max_ret: Maximun return (e.g. 0.09)
  ##nom_pay: Initial Nominal in payment currency.
  ##nom_ori: Nominal in "protection" currency.
              
  dmat=as.numeric(mat_date-ini_date)
  pr_cash_pay=pr_cash_prot=pr_perc_prot=pr_denom_prot=call_pr_pay=bond_pr_pay=bond_pr_prot=index_vol=0
  if(dmat>=0){
  nind=length(index)
  dind=as.numeric(fix_index_date-ini_date)
  dspot=as.numeric(fix_spot_date-ini_date)
  rb=approx_extrap(x=bond_cc_curve[,1],y=bond_cc_curve[,2], xout=dind)$y
  r=unlist(lapply(dom_cc_curve,function(curve){approx_extrap(x=curve[,1],y=curve[,2], xout=dind)$y}))
  rf=unlist(lapply(for_cc_curve,function(curve){approx_extrap(x=curve[,1],y=curve[,2], xout=dind)$y}))
  tn=ifelse(dind<0,0,dind)/base
  strike1=index_ini
  names(strike1)=names(index_ini)=names(index_curr)=index
  
  vol1=sapply(index,function(x){vol_extract(d=dind, strike=strike1[[x]], spot=index_spot[[x]], r=r[[x]], rf=rf[[x]], base=base, vol_matrix=vol_matrix[[x]], quote_delta=quote_delta, quote_bfrr=quote_bfrr, delta_type=delta_type, rate_type=rate_type)})*vol_factor
  vol2=rep(NA,nind)
  if(!is.na(max_ret)){
      strike2=index_ini*(1+max_ret)
      vol2=sapply(index,function(x){vol_extract(d=dind, strike=strike2[[x]], spot=index_spot[[x]], r=r[[x]], rf=rf[[x]], base=base, vol_matrix=vol_matrix[[x]], quote_delta=quote_delta, quote_bfrr=quote_bfrr, delta_type=delta_type, rate_type=rate_type)})*vol_factor
  }
  
  vol_quanto=rep(0,nind);rq=r
  nom_call=rep(nom_pay,nind)
  index_val_conv=index_ini
  index_conv_factor=rep(1,nind)
  ind_quanto=(substr(quanto_id,1,3)!=substr(quanto_id,4,6))
  names(quanto)=names(quanto_spot)=names(quanto_id)=quanto_id
  rq=approx_extrap(x=quanto_cc_curve[,1],y=quanto_cc_curve[,2], xout=dind)$y
  
  if(any(ind_quanto)){
    vol_quanto[ind_quanto]=sapply(quanto_id[ind_quanto],function(x){
                                  r_dom=ifelse(substr(x,4,6)==pay_curr,rq[x==quanto_id],r[x==quanto_id]);
                                  r_for=ifelse(substr(x,4,6)==pay_curr,r[x==quanto_id],rq[x==quanto_id])
                                  vol_extract(d=dind, strike=quanto[[x]], spot=quanto_spot[[x]], r=r_dom, rf=r_for, 
                                  base=base, vol_matrix=vol_matrix_quanto[[x]], quote_delta=TRUE, quote_bfrr=quote_bfrr, delta_type=delta_type, rate_type=rate_type)})
    index_conv_factor[ind_quanto]=sapply(index[ind_quanto], function(x)cash_conv(1,curr_in=index_curr[[x]], spot=quanto[index==x], spot_id=quanto_id[index==x]))
    }
  
  index_val_conv=index_ini*index_conv_factor
  weight_mult=basket_ini*weight/index_val_conv
  weight_conv=weight_mult*index_conv_factor
  
  ##basket_ini=as.vector(t(index_ini)%*%weight_conv)
  ##basket_spot=as.vector(t(index_spot)%*%weight_conv)
  if(is.null(haz_curve)){sd_prob=1}else{sd_prob=sd_prob(dmat, haz_curve)$sp_acum}
  
  r_call=r;
  if(length(obs_dates)==0 |is.null(obs_dates)){
  call_pr2=0
  call_pr1=(nom_pay*b/basket_ini)*bs_basket_mc(ini_date=ini_date,spot=index_spot, strike=strike1, quanto=quanto, weight=weight_conv, v=vol1, vq=vol_quanto, r=r_call, rf=rf, rq=rq, mat_date=mat_date, rho_mat=rho_mat, ini_date_avg=ini_date_avg, fin_date_avg=fin_date_avg, c_p = "call", float_strike=float_strike, geometric=geometric, rate_type = rate_type,  base=base, index_series=index_series, M=M)
  if(!is.na(max_ret)){
    strike2=index_ini*(1+max_ret)
  call_pr2=(nom_pay*b/basket_ini)*bs_basket_mc(ini_date=ini_date,spot=index_spot, strike=strike2, quanto=quanto, weight=weight_conv, v=vol2, vq=vol_quanto,r=r_call, rf=rf, rq=rq, mat_date=mat_date, rho_mat=rho_mat, ini_date_avg=ini_date_avg, fin_date_avg=fin_date_avg, c_p = "call", float_strike=float_strike, geometric=geometric, rate_type = rate_type,  base=base, index_series=index_series, M=M)
    }
  call_pr=call_pr1-call_pr2
  }
  if(all(!is.na(obs_dates)) & length(obs_dates)>0){
    payoff_past=0
    obs_dates_past = obs_dates[obs_dates <= ini_date]
    if (length(obs_dates_past) > 0) {
      index_past = index_series[findInterval(obs_dates_past, index_series[,1]),match(index,colnames(index_series))]
      strike2=index_ini*(1+max_ret)
      payoff_past=sapply(as.matrix(index_past)%*%weight_conv-as.vector(strike1 %*% weight_conv),max,0)
      if(!is.na(max_ret)){
        payoff_past=payoff_past-sapply(as.matrix(index_past)%*%weight_conv-as.vector(strike2 %*% weight_conv),max,0)
      }
      payoff_acum=(nom_pay*b/basket_ini)*sum(payoff_past)
    }
    
      obs_dates_fut = obs_dates[obs_dates > ini_date]
    
    call_pr2=0
    call_pr1=(nom_pay*b/basket_ini)*sapply(obs_dates_fut, bs_basket_mc, ini_date=ini_date,spot=index_spot, strike=strike1, quanto=quanto, weight=weight_conv, v=vol1, vq=vol_quanto, r=r_call, rf=rf, rq=rq, rho_mat=rho_mat, ini_date_avg=ini_date_avg, fin_date_avg=fin_date_avg, c_p = "call", float_strike=float_strike, geometric=geometric, rate_type = rate_type,  base=base, index_series=index_series, M=M)
    if(!is.na(max_ret)){
      strike2=index_ini*(1+max_ret)
      call_pr2=(nom_pay*b/basket_ini)*sapply(obs_dates_fut, bs_basket_mc,ini_date=ini_date,spot=index_spot, strike=strike2, quanto=quanto, weight=weight_conv, v=vol2, vq=vol_quanto,r=r_call, rf=rf, rq=rq, rho_mat=rho_mat, ini_date_avg=ini_date_avg, fin_date_avg=fin_date_avg, c_p = "call", float_strike=float_strike, geometric=geometric, rate_type = rate_type,  base=base, index_series=index_series, M=M)
    } 
  
    call_pr=payoff_acum+sum(call_pr1)-sum(call_pr2)
  }
  
  
  
  fwd_pr=ifelse(dspot>=1,fx_spot+fwd_points[dspot],fx_spot)
  if(pay_curr==nom_curr){fwd_pr=1; fx_spot=1}
  nom_pay_equiv=nom_prot*(a+coupon)
  call_pr_pay=call_pr
  if(pay_curr!=nom_curr){
    nom_pay_equiv=cash_conv(nom_prot*(a+coupon), nom_curr, spot=fwd_pr, spot_id=paste0(pay_curr, nom_curr))
  }
  
  cc_pr=cc_price(days=dmat,rates=rb+credit_spread, base=base,rate_type=rate_type)*(sd_prob+rec*(1-sd_prob))
  cc_rate_pay=disc_rate(cc_pr, days=dmat, base=base, rate_type=rate_type)
  
  bond_pr_pay=nom_pay_equiv*cc_pr
  bond_pr_prot=bond_pr_pay*fx_spot
  pr_cash_pay=bond_pr_pay+call_pr_pay
  pr_cash_prot=pr_cash_pay*fx_spot
  pr_denom_prot=denom*pr_cash_prot/nom_prot
  pr_perc_prot=100*pr_cash_prot/nom_prot
  
  cc_rate_prot=disc_rate(bond_pr_prot/(nom_prot*(a+coupon)), days=dmat, base=base, rate_type=rate_type)
  
  bond_pr_denom=denom*bond_pr_prot/nom_prot
  call_pr_denom=pr_denom_prot-bond_pr_denom
  }
  return(list(pr_cash_pay=round(pr_cash_pay, round_digits), pr_cash_prot=round(pr_cash_prot, round_digits), pr_perc_prot=round(pr_perc_prot, round_digits), pr_denom_prot=round(pr_denom_prot, round_digits), call_pr_pay1=round(call_pr1, round_digits),call_pr_pay2=round(call_pr2, round_digits), 
              bond_pr_pay=round(bond_pr_pay, round_digits), bond_pr_prot=round(bond_pr_prot, round_digits), bond_pr_denom=round(bond_pr_denom, round_digits), call_pr_denom=round(call_pr_denom, round_digits), index_vol1=round(vol1, round_digits), index_vol2=round(vol2, round_digits),
              bond_rate_pay=round(cc_rate_pay, round_digits), bond_rate_prot=round(cc_rate_prot, round_digits), days_mat=dmat, index_spot=round(index_spot, round_digits), quanto_spot=round(quanto_spot, round_digits),vol_quanto=round(vol_quanto, round_digits),
              r_dom=round(r, round_digits),r_for=round(rf, round_digits),r_quanto=round(rq, round_digits)))
}
