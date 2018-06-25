avg_vol<-function(ini_date, index_date, spot, strike, ref_id, vol_matrix, dom_cc_curve, for_cc_curve,ref_spot, ref_weight, base=360, rate_type="nominal", quote_delta=FALSE, quote_bfrr=FALSE, delta_type=1, vol_factor=1){
  
  dind=as.numeric(index_date-ini_date)
  r = unlist(lapply(dom_cc_curve, function(curve) {approx_extrap(x = curve[, 1], y = curve[, 2], xout = dind)$y}))
  rf = unlist(lapply(for_cc_curve, function(curve) {approx_extrap(x = curve[, 1], y = curve[, 2], xout = dind)$y}))
  rel=strike/spot
  strikes=rel*ref_spot
  vols=sapply(ref_id, function(x){
    vol_extract(d = dind, strike = strikes[[x]], spot = ref_spot[[x]], 
                r = r[[x]], rf = rf[[x]], base = base, vol_matrix = vol_matrix[[x]], 
                quote_delta = quote_delta, quote_bfrr = quote_bfrr, 
                delta_type = delta_type, rate_type = rate_type)})*vol_factor
  vol_index=as.numeric(t(ref_weight)%*%vols)
    
  return(vol_index)
}