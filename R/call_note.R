call_note <-function(ini_date, mat_date, fix_index_date, fix_spot_date, ini_date_avg, fin_date_avg,
                     fx_spot, index_spot, index_ini, a, b, nom_prot, bond_cc_curve, dom_cc_curve, for_cc_curve,
                     vol_matrix, base, coupon=0, credit_spread=0, fwd_points, haz_curve=NULL, pay_curr_risk = FALSE,
                     quote_delta=TRUE, quote_bfrr=TRUE, delta_type=1, rate_type="nominal", denom=100, pay_curr="USD",
                     nom_curr="COP", index_curr="USD", round_digits=3, quanto=1, quanto_id=NULL, quanto_spot=NULL,
                     vol_matrix_quanto=NULL, quanto_cc_curve=NULL, rho=0, prev_vol=NULL, adj_strike=0, payoff_type=2,
                     index_series=NULL, float_strike=FALSE, geometric=FALSE, settDays, vol_factor=1, M=1e4, delta_vol_max=0,
                     drift_cero=FALSE, rec=0.4, rb = NA, r = NA, rf = NA, rq = NA, vol_quanto = NA,
                     fwd_pr = NA, opt_pr = NA){
  ##a: Capital Protection (e.g. 0.95)
  ##b: Participation Rate (e.g. 0.5)
  ##nom_pay: Initial Nominal in payment currency.
  ##nom_ori: Nominal in "protection" currency.
  ##si payoff_type==1, strike es index_ini*(1-(1-a)/b); si payoff_type==2, strike es index_ini*(1-(1-a)/b)
  type=ifelse(all(is.na(c(ini_date_avg,fin_date_avg))),"v","a")

  dmat=as.numeric(mat_date-ini_date)
  pr_cash_pay=pr_cash_prot=pr_perc_prot=pr_denom_prot=call_pr_pay=bond_pr_pay=bond_pr_prot=index_vol=0
  if(dmat>=0){
  dind=as.numeric(fix_index_date-ini_date)
  dspot=as.numeric(fix_spot_date-ini_date)
  if(is.na(rb)){
    rb=approx_extrap(x=bond_cc_curve[,1],y=bond_cc_curve[,2], xout=dind)$y
  }
  if(is.na(r)){
    r=approx_extrap(x=dom_cc_curve[,1],y=dom_cc_curve[,2], xout=dind)$y
  }
  if(is.na(rf)){
  rf=approx_extrap(x=for_cc_curve[,1],y=for_cc_curve[,2], xout=dind)$y
  }
  tn=ifelse(dind<0,0,dind)/base
  strike=index_ini*(1-(1-a)/b)
  if((a==0 & b==1)| payoff_type==2){strike=index_ini}
  vol=vol_extract(d=dind, strike=strike+adj_strike, spot=index_spot, r=r, rf=rf, base=base, vol_matrix=vol_matrix, quote_delta=quote_delta, quote_bfrr=quote_bfrr, delta_type=delta_type, rate_type=rate_type)*vol_factor
  if(!is.na(prev_vol) & !is.na(delta_vol_max)){
    delta_vol=vol-prev_vol
    if(abs(delta_vol)>=delta_vol_max){vol=prev_vol+sign(delta_vol)*delta_vol_max}
  }

  if(is.null(haz_curve)){sd_prob=1}else{sd_prob=sd_prob(dmat, haz_curve)$sp_acum}

  nom_pay <- nom_prot
  if(pay_curr!=nom_curr){
    nom_pay=cash_conv(nom_prot, nom_curr, spot=fx_spot, spot_id=paste0(pay_curr, nom_curr))
  }


  nom_call=nom_pay

  if(!pay_curr_risk){
    quanto_call <- quanto
    if(pay_curr!=index_curr){
      if(is.na(rq)){
        rq=approx_extrap(x=quanto_cc_curve[,1],y=quanto_cc_curve[,2], xout=dind)$y
      }
      if(is.na(vol_quanto)){
        vol_quanto=vol_extract(d=dind, strike=quanto, spot=quanto_spot, r=rq, rf=r, base=base, vol_matrix=vol_matrix_quanto, quote_delta=TRUE, quote_bfrr=quote_bfrr, delta_type=delta_type, rate_type=rate_type)
      }
      nom_call=cash_conv(nom_pay,curr_in=pay_curr, spot=quanto, spot_id=quanto_id)
    }else{
      vol_quanto <- 0
      rq <- r
    }

  }else{
      vol_quanto=0
      rq <- r
      rho <- 0
      quanto_call <- quanto_spot
      nom_call=cash_conv(nom_pay,curr_in=pay_curr, spot=quanto, spot_id=quanto_id)
  }

  r_call=r
  if(drift_cero){r_call=rf=0}
  if(type=="v"){
    if(is.na(opt_pr)){
      opt_pr_index_curr <- bs_quanto(spot=index_spot, strike=strike+adj_strike, quanto=1, v=vol, vq=vol_quanto, rho=rho, r=r_call, rf=rf, rq=rq, tn=tn, c_p = "call", rate_type = rate_type)
      opt_pr <- bs_quanto(spot=index_spot, strike=strike+adj_strike, quanto=quanto_call, v=vol, vq=vol_quanto, rho=rho, r=r_call, rf=rf, rq=rq, tn=tn, c_p = "call", rate_type = rate_type)
    }else{
      opt_pr_index_curr <- opt_pr/quanto_call
    }
    call_pr=(nom_call*b/index_ini)*opt_pr
  }
  if(type=="a"){
    if(is.na(opt_pr)){
      opt_pr_index_curr <- bs_asian(ini_date=ini_date,spot=index_spot, strike=strike+adj_strike, quanto=1, v=vol, vq=vol_quanto, rho=rho, r=r_call, rf=rf, rq=rq, mat_date=mat_date, ini_date_avg=ini_date_avg, fin_date_avg=fin_date_avg, c_p = "call", float_strike=float_strike, geometric=geometric, rate_type = rate_type,  base=base, M=M, index_series=index_series, settDays=settDays)
      opt_pr <- bs_asian(ini_date=ini_date,spot=index_spot, strike=strike+adj_strike, quanto=quanto_call, v=vol, vq=vol_quanto, rho=rho, r=r_call, rf=rf, rq=rq, mat_date=mat_date, ini_date_avg=ini_date_avg, fin_date_avg=fin_date_avg, c_p = "call", float_strike=float_strike, geometric=geometric, rate_type = rate_type,  base=base, M=M, index_series=index_series, settDays=settDays)
    }else{
      opt_pr_index_curr <- opt_pr/quanto_call
    }
  }

    call_pr=(nom_call*b/index_ini)*opt_pr


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
  return(list(pr_cash_pay=round(pr_cash_pay, round_digits), pr_cash_prot=round(pr_cash_prot, round_digits), pr_perc_prot=round(pr_perc_prot, round_digits), pr_denom_prot=round(pr_denom_prot, round_digits), call_pr_pay=round(call_pr_pay, round_digits),
              bond_pr_pay=round(bond_pr_pay, round_digits), bond_pr_prot=round(bond_pr_prot, round_digits), bond_pr_denom=round(bond_pr_denom, round_digits), call_pr_denom=round(call_pr_denom, round_digits),
              strike=round(strike, round_digits), index_vol=round(vol, round_digits),
              bond_rate_pay=round(cc_rate_pay, round_digits), bond_rate_prot=round(cc_rate_prot, round_digits), days_mat=dmat, index_spot=round(index_spot, round_digits), quanto_spot=round(quanto_spot, round_digits),vol_quanto=round(vol_quanto, round_digits), rho=round(rho, round_digits),
              r_dom=round(r, round_digits),r_for=round(rf, round_digits),r_quanto=round(rq, round_digits)))
}
