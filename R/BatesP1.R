BatesP1<-function(kv, thetav, sigmav, muj, sigmaj, lambda0, lambda1, p, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf, spot, strike, t){
  int1=integrate(intBatesP1, lower=0, upper=Inf, kv, thetav, sigmav, muj, sigmaj, lambda0, lambda1, p, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf,spot, strike, t)
  P=0.5*cf(1,kv, thetav, sigmav, muj, sigmaj, lambda0, lambda1, p, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf, t)-(1/pi)*int1$value
  return(P)
}
