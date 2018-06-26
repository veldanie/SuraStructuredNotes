betav<-function(const, k, sigmav, muj, sigmaj,lambda1, p, t){
  #p: Corr.
  #mu: Mean relative jump size, i.e. mu=E(exp(U)-1)
  #mu: Amplitud del salto U tiene dist noral con media muj y var sigmaj^2.
  mu=exp(muj+sigmaj^2/2)-1
  b=sigmav*p*const-k
  a=const*(1-const)-2*lambda1*(exp(const*muj+(const^2)*sigmaj^2/2)-1-const*mu)
  gamma=sqrt(b^2+a*(sigmav^2))
  betav=-a*(1-exp(-gamma*t))/(2*gamma-(gamma+b)*(1-exp(-gamma*t)))
  return(betav)
}
