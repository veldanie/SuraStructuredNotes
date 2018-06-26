boots_rates_opt=function (curva, base, cc_days, days_freq, lb = 0, ub = 0.2){
  k=sum(curva[,1]<=cc_days)
  param = curva[-c(1:k), 2]
  l = length(param)
  lowerBounds = rep(lb, l)
  upperBounds = rep(ub, l)
  fit = nlminb(start = param, objective = dif_price_boots,
               gradient = NULL, hessian = NULL, curva, base, cc_days,
               days_freq, lower = lowerBounds, upper = upperBounds)
  return(data.frame(V1=curva[, 1], V2 = c(curva[c(1:k),2],fit$par)))
}
