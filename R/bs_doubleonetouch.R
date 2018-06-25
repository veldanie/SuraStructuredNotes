bs_doubleonetouch <-
function (spot, strike, barrier, v, r, rf, t, c_p = "call", pay_time=0, payoff=1, lim=100, in_out=0, rate_type="nominal") {
    
    cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p ==  "C" | c_p == "Call") * 2 - 1
    if(in_out==1){
      price_doubleonetouch=opbs_pr(spot,strike,v,r,rf,t,cp)
      return(price_doubleonetouch)
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
    Dd=exp(-r_c*t);#Df=exp(-rf_c*t);
    
    h1=log(barrierH/spot)/v;h=h1/sqrt(t)
    l1=log(barrierL/spot)/v;l=l1/sqrt(t)
    #theta1=(r_c-rf_c)/v+v/2
    theta2=(r_c-rf_c)/v-v/2
    #theta11=theta1*sqrt(t)
    theta22=theta2*sqrt(t)
    
    #e1=function(j,h,l){return(2*j*(h-l)-theta11)}
    e2=function(j,h,l){return(2*j*(h-l)-theta22)}
    
    j=seq(from=-abs(lim),to=abs(lim),by=1)
    
    kx=sum(exp(-2*j*theta22*(h-l))*(pnorm(h+e2(j,h,l))-pnorm(l+e2(j,h,l)))-exp(-2*j*theta22*(h-l)+2*theta22*h)*(pnorm(h-2*h+e2(j,h,l))-pnorm(l-2*h+e2(j,h,l))))
  return(Dd-Dd*kx)
  }
