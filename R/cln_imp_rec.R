cln_imp_rec<-function(cln_price, ini_date, coupon, freq, mat_date, haz_curve, ref_curr, nom_curr, nom=1, conv="act_360", fx_spot=1, fwd_points=0, factor_fwd=1, serie_float_rate = NULL, disc_curve=NULL, base_disc=360, proj_curve= NULL, base_proj=360, denom=100, spread=0, round_digits=5, base_currs=c("NZD","AUD","EUR","GBP"),interv=c(0,1)){
  
  rec=0
    obj_fun_cln<-function(x){
      diff=cln_price-cln(ini_date=ini_date, coupon=coupon, freq=freq, mat_date=mat_date, haz_curve=haz_curve, ref_curr=ref_curr, nom_curr=nom_curr, nom=nom, rec=x, conv=conv, fx_spot=fx_spot, fwd_points=fwd_points, factor_fwd=factor_fwd, serie_float_rate = serie_float_rate, disc_curve=disc_curve, base_disc=base_disc, proj_curve=proj_curve, base_proj=base_proj, denom=denom, spread=spread, round_digits=round_digits, base_currs=base_currs)$pr_perc
      return(diff)
    }
    rec=try(uniroot(f=obj_fun_cln, interval=interv)$root, silent=TRUE)
    if(!is.numeric(rec)){rec=0}
  return(rec)
}
