cf<-function(const, kv, thetav, sigmav, muj, sigmaj, lambda0, lambda1, p, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf, t){
  ar=alphar(const,kr,thetar,sigmar, t); br=betar(const, kr, sigmar,t)
  af=alphaf(const,kf,thetaf,sigmaf, t); bf=betaf(const, kf, sigmaf, t)
  av=alphav(const, kv, thetav, sigmav,  muj, sigmaj, lambda0, lambda1, p, t)
  bv=betav(const, kv, sigmav, muj, sigmaj,lambda1, p, t)
  A=ar+af+av
  cfun=exp(A+bv*var+br*r+bf*rf)
  return(cfun)
}
