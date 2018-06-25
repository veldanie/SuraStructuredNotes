bs_lookback_cont <-
function (spot, strike, m,  v, r, rf, t, c_p = "call", float_strike=1, rate_type="nominal") {
    #m:= Maximo - minimo
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

    I=0
    if(float_strike==1){k=m;n=1
    }else if(float_strike==0 & cp*strike>=cp*m){k=strike;n=-1
    }else if(float_strike==0 & cp*strike<cp*m){k=m;I=1;n=-1}

    d1=log(spot/k+(r_c-rf_c+0.5*v^2)*t)/(v*sqrt(t))
    d2=d1-v*sqrt(t)

    price_lookback=cp*(I*Dd*cp*(k-strike)+spot*Df*pnorm(cp*d1)-Dd*k*pnorm(cp*d2)+
                   n*Dd*(v2^2/(2*(r_c-rf_c)))*spot*((spot/k)^(2*(r_c-rf_c)/v^2)*pnorm(n*cp*(-d1+2*(r_c-rf_c)*sqrt(t)/v))-
                   exp((r_c-rf_c)*t)*pnorm(-n*cp*d1)))
    return(price_lookback)
  }
