bs_basket_closed <-
function (spot, strike, weights, cov, r, rf, t0, t, c_p = "call", rate_type="nominal"){
      #Pricing using lognormal taylor expansion (Ju (2002))
      #spot, strike, weights, r, rf: Vectores COLUMNA de longitud al numero de subyancentes.
      #cov=Covariance matrix.
      if (length(spot)!=length(strike) |length(spot)!=length(weights) |length(spot)!=length(v) |length(spot)!=length(r) |length(spot)!=length(rf)){
      stop("Longitud de los vectores es diferente")}
      cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p ==
              "C" | c_p == "Call") * 2 - 1

      r_c = r
      rf_c= rf
      if (substr(rate_type, 1, 1) == "n") {
        r_c = log(1 + r * t)/t
        rf_c = log(1 + rf * t)/t
      }
      if (substr(rate_type, 1, 1) == "e") {
        r_c = log(1 + r)
        rf_c = log(1 + rf)
      }
      Dd=exp(-r_c*t);Df=exp(-rf_c*t);
      wspot=weights*spot*exp((r_c-rf_c)*t)
      U1=sum(wspot)
      V=t(wspot)%*%exp(cov)%*%wspot
      m=2*log(U)-0.5*log(V)
      var=log(V)-2*log(U)
      U2=sum(wspot%*%t(wspot))
      U21=t(wspot)%*%cov%*%wspot
      U22=t(wspot)%*%(cov*cov)%*%wspot
      U23=(t(wspot)%*%(cov*cov*cov)%*%wspot)
      a1=-0.5*U21/U2
      a2=2*a1^2-0.5*U22/U2
      a3=6*a1*a2-4*a1^3-0.5*U23/U2
      b1=0;lw=length(wspot)
      for(i in c(1:lw)){
        for(j in c(1:lw)){
          for(k in c(1:lw)){
            b1=b1+wspot[i]*wspot[j]*wspot[k]*cov[i,k]*cov[j,k]}}}
      b1=2*b1/(4*U1^3)
      b3=a1^2-0.5*a2
      c1=-a1*b1
      c21=0;
      for(i in c(1:lw)){
        for(j in c(1:lw)){
          for(k in c(1:lw)){
            for(l in c(1:lw)){
              c21=c21+wspot[i]*wspot[j]*wspot[k]*wspot[l]*cov[i,l]*cov[j,k]*cov[k,l]}}}}
      c22=0
      for(i in c(1:lw)){
        for(j in c(1:lw)){
          for(k in c(1:lw)){
            for(l in c(1:lw)){
              c22=c22+wspot[i]*wspot[j]*wspot[k]*wspot[l]*cov[i,l]*cov[j,l]*cov[k,l]}}}}

      c2=(1/(144*U1^4))*(9*(8*c21+2*U21*U22)+4*(6*c22))

      c31=0;
      for(i in c(1:lw)){
        for(j in c(1:lw)){
          for(k in c(1:lw)){
              c31=c31+wspot[i]*wspot[j]*wspot[k]*cov[i,k]*cov[j,k]^2}}}
      c32=0
      for(i in c(1:lw)){
        for(j in c(1:lw)){
          for(k in c(1:lw)){
              c32=c32+wspot[i]*wspot[j]*wspot[k]*cov[i,j]*cov[i,k]*cov[j,k]}}}
      c3=(1/(48*U1^3))*(4*6*c31+8*c32)
      c4=a1*a2-(2/3)*a1^3-(1/6)*a3

      d1=0.5*(6*a1^2+a2-4*b1+2*b2)-(1/6)*(120*a1^3-a3+6*(24*c1-6*c2+2*c3-c4))
      d2=0.5*(10*a1^2+a2-6*b1+2*b2)-(128*a1^3/3-a3/6+2*a1*b1-a1*b2+50*c1-11*c2+3*c3-c4)
      d3=2*a1^2-b1-(1/3)*(88*a1^3+3*a1*(5*b1-2*b2))
      d4=(-20*a1^3/3+a1*(-4*b1+b2-10*c1+c2))
      #Validar d1-d2+d3-d4=0
      z1=d2-d3+d4
      z2=d3-d4
      z3=d4
      y=log(strike);y1=(m-y)/sqrt(var);y2=y1-sqrt(var)
      price_basket=Dd*U1*pnorm(y1)-strike*Dd*pnorm(y2)+Dd*strike*(z1*pnorm(y)+z2*+z3)
    return(price_basket)
    }
