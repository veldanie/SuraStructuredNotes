intBatesP2<-function(u, kv, thetav, sigmav, muj, sigmaj, lambda0, lambda1, p, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf, spot, strike, t){
  lx=log(strike/spot)
  ib2=Im(cf(complex(real=0,imaginary=-u),kv, thetav, sigmav, muj, sigmaj, lambda0, lambda1, p, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf, t)*exp(complex(real=0, imaginary=u*lx)))/u
  return(ib2)
}
