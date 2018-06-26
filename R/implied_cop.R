implied_cop<-function(cc_lib_usd, fwd_points, TRM, base){
  m=length(cc_lib_usd)
  days=c(1:m)
  imp_cop=((TRM+fwd_points)*(1+cc_lib_usd*days/base)/TRM-1)*base/days
  return(imp_cop)
}
