bs_barrier <-
function (spot, strike, barrier, v, r, rf, t, c_p = "call", rebate=0, in_out=0,rate_type="nominal") {
    #Si rebate es diferente de 0 hay que adherir una opcion digital.
    #in_out: Indicates if the option has knocked in or knocked out in the past.

    cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p == "C" | c_p == "Call") * 2 - 1
    if(in_out==1){
      price_barr = list(inn=opbs_pr(spot,strike,v,r,rf,t,cp), out=0)
      return(price_barr)
    }

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

    Df=exp(-rf_c*t);Dd=exp(-r_c*t);
    d1 = (log(spot/strike) + (r_c - rf_c + v^2/2) * t)/(v * sqrt(t))
    d2 = d1 - v * sqrt(t)
    x1 = (log(spot/barrier) + (r_c - rf_c + v^2/2) * t)/(v * sqrt(t))
    x2 = x1 - v * sqrt(t)
    z1 = (log(barrier^2/(spot*strike)) + (r_c - rf_c + v^2/2) * t)/(v * sqrt(t))
    z2 = z1 - v * sqrt(t)
    y1 = (log(barrier/spot) + (r_c - rf_c + v^2/2) * t)/(v * sqrt(t))
    y2 = y1 - v * sqrt(t)


    A1=cp*Df*spot*pnorm(cp*d1)-cp*Dd*strike*pnorm(cp*d2)
    A2=cp*Df*spot*pnorm(cp*x1)-cp*Dd*strike*pnorm(cp*x2)

    A3u=cp*(barrier/spot)^(2*((r_c-rf_c)/v-v/2)/v)*(spot*Df*(barrier/spot)^2*pnorm(-1*z1)-strike*Dd*pnorm(-1*z2))
    A4u=cp*(barrier/spot)^(2*((r_c-rf_c)/v-v/2)/v)*(spot*Df*(barrier/spot)^2*pnorm(-1*y1)-strike*Dd*pnorm(-1*y2))

    A3d=cp*(barrier/spot)^(2*((r_c-rf_c)/v-v/2)/v)*(spot*Df*(barrier/spot)^2*pnorm(z1)-strike*Dd*pnorm(z2))
    A4d=cp*(barrier/spot)^(2*((r_c-rf_c)/v-v/2)/v)*(spot*Df*(barrier/spot)^2*pnorm(y1)-strike*Dd*pnorm(y2))

    if(cp==1 & strike>barrier & spot<barrier){
      price_barr = list(inn=A1, out=0)
    }
    if(cp==1 & strike>barrier & spot>=barrier){
      price_barr = list(inn=A3d, out=A1-A3d)
    }

    if(cp==1 & strike<=barrier & spot<barrier){
      price_barr = list(inn=A2-A3u+A4u, out=A1-A2+A3u-A4u)
    }
    if(cp==1 & strike<=barrier & spot>=barrier){
      price_barr = list(inn=A1-A2+A4d, out=A2-A4d)
    }
    if(cp==-1 & strike>barrier & spot<barrier){
      price_barr = list(inn=A1-A2+A4u, out=A2-A4u)}
    if(cp==-1 & strike>barrier & spot>=barrier){
      price_barr = list(inn=A2-A3d+A4d, out=A1-A2+A3d-A4d)}

    if(cp==-1 & strike<=barrier & spot<barrier){
      price_barr = list(inn=A3u, out=A1-A3u)}

    if(cp==-1 & strike<=barrier & spot>=barrier){
      price_barr = list(inn=A1, out=0)}

    return(price_barr)
  }
