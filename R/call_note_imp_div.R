call_note_imp_div <-
function(note_price, ini_date, mat_date, fix_index_date, fix_spot_date, fx_spot, index_spot, index_ini, a, b, nom_pay, nom_prot, bond_cc_curve, dom_cc_curve, for_cc_curve, vol_matrix, base, coupon=0, fwd_points, haz_curve=NULL, pay_curr_risk=FALSE, quote_delta=TRUE, quote_bfrr=TRUE, delta_type=1, rate_type="nominal", denom=100, pay_curr="USD", nom_curr="COP", index_curr="USD", round_digits=3,quanto=1, quanto_id=NULL, quanto_spot=NULL, vol_matrix_quanto=NULL, quanto_cc_curve=NULL, rho=0,prev_vol=NULL, interv=c(-0.2,0.2)){
  div=0
    obj_fun_ne<-function(x){
      for_curve=data.frame(c(1,3600),x)
      diff=note_price-call_note(ini_date, mat_date, fix_index_date, fix_spot_date, fx_spot, index_spot, index_ini, a, b, nom_pay, nom_prot, bond_cc_curve, dom_cc_curve, for_curve, vol_matrix, base, coupon, credit_spread=0, fwd_points, haz_curve=haz_curve, pay_curr_risk=pay_curr_risk, quote_delta=quote_delta, quote_bfrr=quote_bfrr, delta_type=delta_type, rate_type=rate_type, denom=denom, pay_curr=pay_curr, nom_curr=nom_curr, index_curr=index_curr, round_digits=round_digits,quanto=quanto, quanto_id=quanto_id, quanto_spot=quanto_spot, vol_matrix_quanto=vol_matrix_quanto, quanto_cc_curve=quanto_cc_curve, rho=rho, prev_vol=prev_vol)$pr_perc_prot
      return(diff)
    }
    div=try(uniroot(f=obj_fun_ne, interval=interv)$root, silent=TRUE)
    if(!is.numeric(div)){div=0}
  div_cont=log(1+div*as.numeric(mat_date-ini_date)/360)*360/as.numeric(mat_date-ini_date)
  return(div_cont)
}
