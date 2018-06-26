implied_inter<-function(cc_cop, fwd_points, Spot, base_ref, base_imp){
  m=min(length(cc_cop), length(fwd_points))
  days=c(1:m)
  imp_inter=((1+cc_cop[1:m]*days/base_ref)/(1+fwd_points[1:m]/Spot)-1)*base_imp/days
  return(imp_inter)
}
