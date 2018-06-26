vola_obj<-function(x, price, spot, strike, r, rf, tn, c_p, tipo_tasa){
  diff_opc_pr=opbs_pr(spot, strike, x, r, rf, tn, c_p, tipo_tasa)-price
  return(diff_opc_pr)
}
