bs_onetouch <-
function (payoff=1, spot, strike, barrier, v, r, rf, t, c_p = "call", pay_time=0, in_out=0, rate_type="nominal") {
  #Pays domestic cash amount if barrier is hit before expiration. 
  #pay_time=0, then payment is at first hitting time. pay_time=1, then payment is at maturity.
    
    
    cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p == "C" | c_p == "Call") * 2 - 1
    if(in_out==1){
      price_onetouch = opbs_pr(spot,strike,v,r,rf,t,cp)
      return(price_onetouch)}
    
    if(B<spot){n=1};
    if(B>spot){n=-1}#Lower or upper barrier
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
    
    theta=(r_c-rf_c)/v-v/2;
    theta1=sqrt(theta^2+2*(1-pay_time)*r_c)
    e1=(log(spot/barrier)-v*theta1*t)/(v*sqrt(t))  
    e2=(-log(spot/barrier)-v*theta1*t)/(v*sqrt(t))  
    price_onetouch=payoff*exp(-pay_time*r_c*t)*((barrier/spot)^((theta1+theta)/v)*pnorm(-n*e1)+(barrier/spot)^((theta1-theta)/v)*pnorm(n*e2))
    return(price_onetouch)
  }
