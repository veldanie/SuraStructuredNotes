betaf<-function(const, k, sigma, t){
  gamma=sqrt(k^2+2*const*sigma^2)
  betaf=-2*const*(1-exp(-gamma*t))/(2*gamma-(gamma-k)*(1-exp(-gamma*t)))
  return(betaf)
}
