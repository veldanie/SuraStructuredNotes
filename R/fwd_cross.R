fwd_cross<-function(base_curr, ref_curr, spot, factor_fwd, curr_mkt1, pr_fwd1, curr_mkt2, pr_fwd2){
  if (any(base_curr== "USD") || any(ref_curr == "USD"))
    stop("Esta funci?n aplica para pares de monedas donde la base y la referencia son diferentes a USD")

  lf=min(length(pr_fwd1$V1),length(pr_fwd2$V1))

  mkt_codes=c(substr(curr_mkt1,1,3),substr(curr_mkt1,4,6),substr(curr_mkt2,1,3), substr(curr_mkt2,4,6))
  base_pos=match(base_curr,mkt_codes)
  ref_pos=match(ref_curr,mkt_codes)

  if (any(base_pos==1) & any(ref_pos==4)){
    fwd_cross_mid=((pr_fwd1$V1[1:lf])*(pr_fwd2$V1[1:lf])-spot)/factor_fwd
    fwd_cross_bid=((pr_fwd1$V2[1:lf])*(pr_fwd2$V2[1:lf])-spot)/factor_fwd
    fwd_cross_ask=((pr_fwd1$V3[1:lf])*(pr_fwd2$V3[1:lf])-spot)/factor_fwd
  }

  if (any(base_pos==2) & any(ref_pos==4)){
    fwd_cross_mid=((1/pr_fwd1$V1[1:lf])*(pr_fwd2$V1[1:lf])-spot)/factor_fwd
    fwd_cross_bid=((1/pr_fwd1$V3[1:lf])*(pr_fwd2$V2[1:lf])-spot)/factor_fwd
    fwd_cross_ask=((1/pr_fwd1$V2[1:lf])*(pr_fwd2$V3[1:lf])-spot)/factor_fwd
  }
  fwd_cr=data.frame(V1=fwd_cross_mid, V2=fwd_cross_bid, V3=fwd_cross_ask)
  return(fwd_cr)
}

