betar<-function(const, k, sigma, t){
  gamma=sqrt(k^2+2*(1-const)*sigma^2)
  betar=-2*(1-const)*(1-exp(-gamma*t))/(2*gamma-(gamma-k)*(1-exp(-gamma*t)))
  return(betar)
}
