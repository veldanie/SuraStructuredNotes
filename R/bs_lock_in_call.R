bs_lock_in_call<-function(ini_date, spot, strike, quanto=1, v, vq=0, rho=0, r, rf, rq, mat_date, li_levels, obs_dates, rate_type="nominal", base=360, M=1e3, index_series=NULL, vol_factor=1){
   
  #t:= Time to maturity.
   #t0:=Time between valuation date and averaging start. t0 belongs to (-inf, inf)
    tn=as.numeric(mat_date-ini_date)/base
    
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
      obs_dates_past=obs_dates[obs_dates<=ini_date]
      st_perf_past=0
      if(length(obs_dates_past)>0){
        index_past=index_series[match(obs_dates_past,index_series[,1]),2]
        st_perf_past=index_past/strike
      }  
    
      obs_dates_fut=obs_dates[obs_dates>ini_date]
      days_seq=as.numeric(obs_dates_fut-ini_date)
      
      t_seq=days_seq/base
      l_seq=length(t_seq)
      v=v*vol_factor  
      drift=r-rf-rho*v*vq
    
      st=spot_simul(spot=spot, exp_times=t_seq, rates=drift, sigma=v, m=M) 
      st_perf_mat=st/strike
      st_perf=as.vector(tail(st_perf_mat,1))
      st_ret=st_perf-1
      
      locked_perf=sapply(findInterval(st_perf,li_levels),function(x)ifelse(x==0,0,li_levels[x]-1))
    
      max_st_perf=apply(rbind(repc(st_perf_past,M),st_perf_mat),2,max)
      min_level=min(li_levels)  
    
      payoff=rep(0,M)
      payoff[max_st_perf<min_level]=sapply(st_ret[max_st_perf<min_level]*(st_ret[max_st_perf<min_level]>=0),min,max_ret)
      payoff[max_st_perf>=min_level]=sapply(apply(cbind(locked_perf[max_st_perf>=min_level],st_ret[max_st_perf>=min_level]),1,max),min,max_ret)
    
      mean_payoff=mean(payoff)
      price_call=exp(-rq*tn)*quanto*mean_payoff
    
    return(price_call)
  }
