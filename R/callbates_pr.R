callbates_pr<-function(kv, thetav, sigmav, muj, sigmaj, lambda0, lambda1, p, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf, spot, strike, t){
  x=strike/spot
  price=BatesP1(kv, thetav, sigmav, muj, sigmaj, lambda0, lambda1, p, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf, spot, strike, t)-x*BatesP2(kv, thetav, sigmav, muj, sigmaj, lambda0, lambda1, p, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf, spot, strike, t)
  return(spot*price)
}
