
bs_asian <-function (ini_date, spot, strike, quanto=1, v, vq=0, rho=0, r, rf, rq, mat_date, ini_date_avg, fin_date_avg, c_p = "call", float_strike=FALSE, geometric=TRUE, rate_type="nominal", base=360, M=1e3, index_series=NULL, settDays){
   
  #t:= Time to maturity.
   #t0:=Time between valuation date and averaging start. t0 belongs to (-inf, inf)
    tn=as.numeric(mat_date-ini_date)/base
    t0=as.numeric(ini_date_avg-ini_date)/base
    cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p =="C" | c_p == "Call") * 2 - 1
    
    r_c = r
    rf_c= rf
    rq_q=rq
    if (substr(rate_type, 1, 1) == "n") {
      r_c = log(1 + r * tn)/tn
      rf_c = log(1 + rf * tn)/tn
      rq_c = log(1 + rq * tn)/tn
    }
    if (substr(rate_type, 1, 1) == "e") {
      r_c = log(1 + r)
      rf_c = log(1 + rf)
      rq_c = log(1 + rq)
    }
    Dd=exp(-r_c*tn);Df=exp(-rf_c*tn);
    alpha=tn/(t0+tn)
    
    avgt0=0
    if(t0<=0){
      max_date_avg=min(fin_date_avg,ini_date)
      avgt0=mean(index_series[findInterval(settDays[findInterval(ini_date_avg,settDays):findInterval(ini_date,settDays)],index_series[,1]),2])
    }
    
    
    x=alpha*((1+r*tn)^alpha-1)/(alpha*r*tn)*r
    if(geometric==TRUE & float_strike==FALSE){
    if (t0<0){#Asian option - previous average start
    t0=-t0
    H=exp(-alpha*tn*(r_c-rf_c+v^2*(1-2*alpha/3)/2)/2)
    vanilla=opbs_pr(spot,(strike/H)*(spot/avgt0)^(1-alpha),alpha*v/sqrt(3),alpha*((1+r*tn)^alpha-1)/(alpha*r*tn)*r,alpha*((1+rf*tn)^alpha-1)/(alpha*rf*tn)*rf,tn,c_p)
    price_asian=exp((alpha-1)*r_c*tn)*H*(spot/avgt0)^(alpha-1)*vanilla
    }
    if (t0>0){#Asian option - forward average start
    theta=(r_c-rf_c)/v-v/2;sigma=sqrt(tn^3/3+2*t0^3/3-t0^2*tn)
    H=exp(-v*theta*(tn-t0)/2-v^2*(t0-sigma/(tn-t0))/2)
    vanilla=opbs_pr(spot,strike/H,sigma*v/((tn-t0)*sqrt(tn)),r,rf,tn,c_p)
    price_asian=H*vanilla  
    }
    }
    if (geometric==FALSE){
      days_seq=as.numeric(settDays[findInterval(ini_date_avg,settDays):findInterval(fin_date_avg,settDays)]- ini_date)
      days_seq=days_seq[days_seq>0]
      t_seq=days_seq/base
      l_seq=length(t_seq)
      drift=r-rf-rho*v*vq
      days_acum_avg=ifelse(ini_date_avg>ini_date, 0,length(settDays[findInterval(ini_date_avg,settDays):findInterval(ini_date,settDays)]))
      st=spot_simul(spot=spot, exp_times=t_seq, rates=drift, sigma=v, m=M) 
      st_avg=(apply(st,2,sum)+avgt0)/(l_seq+days_acum_avg)
      if(float_strike==TRUE){
        mean_payoff=mean(sapply(cp*(st[l_seq,]-st_avg),max,0))
      }
      if(float_strike==FALSE){
        mean_payoff=mean(sapply(cp*(st_avg-strike),max,0))
      }
      price_asian=exp(-rq*tn)*quanto*mean_payoff
    } #Asian option - previous average start
    return(price_asian)
  }
