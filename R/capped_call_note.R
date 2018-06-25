capped_call_note <-function(ini_date, mat_date, fix_index_date, fix_spot_date, ini_date_avg, fin_date_avg, fx_spot, index_spot, index_ini, a, b, max_ret,
                            nom_prot, bond_cc_curve, dom_cc_curve, for_cc_curve, vol_matrix, base, coupon=0, credit_spread=0,
                            fwd_points,  haz_curve=NULL, pay_curr_risk=FALSE, quote_delta=TRUE, quote_bfrr=TRUE, delta_type=1, rate_type="nominal",
                            denom=100, pay_curr="USD", nom_curr="COP", index_curr="USD", round_digits=3, quanto=1, quanto_id=NULL, quanto_spot=NULL,
                            vol_matrix_quanto=NULL, quanto_cc_curve=NULL, rho=0, prev_vol1=NULL,prev_vol2=NULL, float_strike=FALSE, geometric=FALSE,index_series=NULL, settDays, vol_factor=c(1,1), delta_vol_max=NA, rec=0.4){

  ##a: Capital Protection (e.g. 0.95)
  ##b: Participation Rate (e.g. 0.5)
  ##max_ret: Maximun return (e.g. 0.09)
  ##nom_pay: Initial Nominal in payment currency.
  ##nom_ori: Nominal in "protection" currency.
  type <- ifelse(all(is.na(c(ini_date_avg,fin_date_avg))),"v","a")

  dmat <- as.numeric(mat_date-ini_date)
  pr_cash_pay <- pr_cash_prot <- pr_perc_prot <- pr_denom_prot <- call_pr_pay <- bond_pr_pay <- bond_pr_prot <- index_vol <- 0
  if(dmat>=0){
  dind=as.numeric(fix_index_date-ini_date)
  dspot=as.numeric(fix_spot_date-ini_date)
  rb=approx_extrap(x=bond_cc_curve[,1],y=bond_cc_curve[,2], xout=dind)$y
  r=approx_extrap(x=dom_cc_curve[,1],y=dom_cc_curve[,2], xout=dind)$y
  rf=approx_extrap(x=for_cc_curve[,1],y=for_cc_curve[,2], xout=dind)$y
  tn=ifelse(dind<0,0,dind)/base
  #strike1=index_ini
  strike1=index_ini*(1-(1-a)/b)
  strike2=index_ini*(1+max_ret)

  vol1=vol_extract(d=dind, strike=strike1, spot=index_spot, r=r, rf=rf, base=base, vol_matrix=vol_matrix, quote_delta=quote_delta, quote_bfrr=quote_bfrr, delta_type=delta_type, rate_type=rate_type)*vol_factor[1]
  vol2=vol_extract(d=dind, strike=strike2, spot=index_spot, r=r, rf=rf, base=base, vol_matrix=vol_matrix, quote_delta=quote_delta, quote_bfrr=quote_bfrr, delta_type=delta_type, rate_type=rate_type)*vol_factor[2]

  if(!is.null(prev_vol1) & !is.null(prev_vol2) & !is.na(delta_vol_max)){
    delta_vol1=vol1-prev_vol1
    delta_vol2=vol2-prev_vol2
    if(abs(delta_vol1)>=delta_vol_max){vol1=prev_vol1+sign(delta_vol1)*delta_vol_max}
    if(abs(delta_vol2)>=delta_vol_max){vol2=prev_vol2+sign(delta_vol2)*delta_vol_max}
  }
  
  nom_pay <- nom_prot
  if(pay_curr!=nom_curr){
    nom_pay=cash_conv(nom_prot, nom_curr, spot=fx_spot, spot_id=paste0(pay_curr, nom_curr))
  }
  
  
  vol_quanto=0;rq=r
  nom_call=nom_pay
  if(pay_curr!=index_curr){
    rq=approx_extrap(x=quanto_cc_curve[,1],y=quanto_cc_curve[,2], xout=dind)$y
    vol_quanto=vol_extract(d=dind, strike=quanto, spot=quanto_spot, r=rq, rf=r, base=base, vol_matrix=vol_matrix_quanto, quote_delta=TRUE, quote_bfrr=quote_bfrr, delta_type=delta_type, rate_type=rate_type)
    nom_call=cash_conv(nom_pay,curr_in=pay_curr, spot=quanto, spot_id=quanto_id)
  }
  if(is.null(haz_curve)){sd_prob=1}else{sd_prob=sd_prob(dmat, haz_curve)$sp_acum}

  r_call=r
  if(type=="v"){
    call_pr1=(nom_call*b/index_ini)*bs_quanto(spot=index_spot, strike=strike1, quanto=quanto, v=vol1, vq=vol_quanto, rho=rho, r=r_call, rf=rf, rq=rq, tn=tn, c_p = "call", rate_type = rate_type)
    call_pr2=(nom_call*b/index_ini)*bs_quanto(spot=index_spot, strike=strike2, quanto=quanto, v=vol2, vq=vol_quanto, rho=rho, r=r_call, rf=rf, rq=rq, tn=tn, c_p = "call", rate_type = rate_type)
  }
  if(type=="a"){
    call_pr1=(nom_call*b/index_ini)*bs_asian(ini_date=ini_date,spot=index_spot, strike=strike1, quanto=quanto, v=vol1, vq=vol_quanto, rho=rho, r=r_call, rf=rf, rq=rq, mat_date=mat_date, ini_date_avg=ini_date_avg, fin_date_avg=fin_date_avg, c_p = "call", float_strike=float_strike, geometric=geometric, rate_type = rate_type,  base=base, M=1e3, index_series=index_series, settDays=settDays)
    call_pr2=(nom_call*b/index_ini)*bs_asian(ini_date=ini_date,spot=index_spot, strike=strike2, quanto=quanto, v=vol2, vq=vol_quanto, rho=rho, r=r_call, rf=rf, rq=rq, mat_date=mat_date, ini_date_avg=ini_date_avg, fin_date_avg=fin_date_avg, c_p = "call", float_strike=float_strike, geometric=geometric, rate_type = rate_type,  base=base, M=1e3, index_series=index_series, settDays=settDays)
  }

  call_pr=call_pr1-call_pr2

  fwd_pr=ifelse(dspot>=1 & length(fwd_points)>1,fx_spot+approx_extrap(x=1:length(fwd_points),y=fwd_points,xout=dspot)$y,fx_spot)
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
              bond_rate_pay=round(cc_rate_pay, round_digits), bond_rate_prot=round(cc_rate_prot, round_digits), days_mat=dmat, index_spot=round(index_spot, round_digits), quanto_spot=round(quanto_spot, round_digits),vol_quanto=round(vol_quanto, round_digits), rho=round(rho, round_digits),
              r_dom=round(r, round_digits),r_for=round(rf, round_digits),r_quanto=round(rq, round_digits)))
}
