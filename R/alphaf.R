alphaf<-function(const, k, theta, sigma, t){
  gamma=sqrt(k^2+2*const*sigma^2)
  alphaf=-k*theta*((gamma-k)*t+2*log(1-(gamma-k)*(1-exp(-gamma*t))/(2*gamma)))/sigma^2
  return(alphaf)
}
