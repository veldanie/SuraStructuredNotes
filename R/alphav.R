alphav<-function(const, k, theta, sigmav, muj, sigmaj, lambda0, lambda1, p, t){
  #p: Corr.
  #mu: Mean relative jump size, i.e. mu=E(exp(U)-1)
  #muj: Amplitud del salto U tiene dist normal con media muj y var sigmaj^2.
  mu=exp(muj+sigmaj^2/2)-1
  b=sigmav*p*const-k
  a=const*(1-const)-2*lambda1*(exp(const*muj+const^2*sigmaj^2/2)-1-const*mu)
  gamma=sqrt(b^2+a*(sigmav^2))

  alphav=-k*theta*((gamma+b)*t+2*log(1-(gamma+b)*(1-exp(-gamma*t))/(2*gamma)))/sigmav^2+lambda0*t*(exp(const*muj+(const^2)*(sigmaj^2)/2)-1-const*mu)
  return(alphav)
}
