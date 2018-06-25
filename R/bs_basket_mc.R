bs_basket_mc <-function (ini_date, spot, strike, quanto=1, weight,v, vq = 0,
                         r, rf, rq, mat_date, rho_mat, ini_date_avg=NA, fin_date_avg=NA, c_p = "call",
                         float_strike = FALSE, geometric = TRUE, rate_type = "nominal",
                         base = 360, index_series = NULL, M = 1000){
    #Pricing using multivariate geometric brownian motion.
    #spot, strike, weights, r, rf: Vectores COLUMNA de longitud al numero de subyancestes.
    #sigma= vector de sigma.
    #rho= matriz de correlaciones.
    #cov=Covariance matrix.

    type=ifelse(all(is.na(c(ini_date_avg,fin_date_avg))),"v","a")

    cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p == "C" | c_p == "Call") * 2 - 1
    r_c = r
    rf_c= rf
    rq_c= rq
    tn = as.numeric(mat_date - ini_date)/base

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
    nind=length(spot)
    rho=rep(0,nind)
    for(h in 1:nind){
      if(substr(quanto_id[h],1,3)!=substr(quanto_id[h],4,6)){
        sign_rho=2*(index_curr[h]==substr(quanto_id[h],1,3))-1
        rho[h]=sign_rho*as.numeric(rho_mat[which(quanto_id[h]==colnames(rho_mat)),which(index[h]==colnames(rho_mat))])
      }
    }

    rho_index=rho_mat[match(index,colnames(rho_mat)),match(index,colnames(rho_mat))]

    drift=r-rf-rho*v*vq
    basket_strike=as.vector(strike%*%weight)
    if(type=="v"){
      st=spot_simul_mult(spot=spot, times=tn, rates=drift, sigma=v, rho=rho_index, M=M)
      it=sapply(1:M, function(x)st[,x,]%*%weight)
      diff_it=cp*(it-basket_strike)
      payoff=diff_it*(diff_it>0)
      if(is.matrix(payoff)){payoff=apply(payoff,2,max)}
      price=exp(-rq_c*tn)*mean(payoff)
    }
    if(type=="a"){

      t0=as.numeric(ini_date_avg-ini_date)/base

      avgt0=0
      if(t0<=0){
        max_date_avg=min(fin_date_avg,ini_date)
        avgt0=mean(as.matrix(index_series[index_series[,1]>=ini_date_avg & index_series[,1]<=max_date_avg,match(index,colnames(index_series))])%*%weight)
      }

    if(geometric==TRUE){stop("S?lo se valoran opciones de promedio aritm?tico.")}
    if (geometric==FALSE){
      days_seq=as.numeric(seq(ini_date_avg,fin_date_avg,by="1 day")-ini_date)
      days_seq=days_seq[days_seq>0]
      t_seq=days_seq/base
      l_seq=length(t_seq)
      drift=r-rf-rho*v*vq
      days_acum_avg=ifelse(ini_date_avg>ini_date, 0,as.numeric(ini_date-ini_date_avg)+1)

      st=spot_simul_mult(spot=spot, times=t_seq, rates=drift, sigma=v, rho=rho_index, M=M)
      ##it_avg=(sapply(1:M, function(x)sum(st[,x,]%*%weight))+avgt0)/(l_seq + days_acum_avg)
      it_avg=(sapply(1:M, function(x)apply(st[,x,]%*%weight,2,sum))+avgt0)/(l_seq + days_acum_avg)
      it=sapply(1:M, function(x)st[l_seq,x,]%*%weight)
      if(float_strike==TRUE){
        diff_it=cp*(it-it_avg)
        payoff=diff_it*(diff_it>0)
        if(is.matrix(payoff)){payoff=apply(payoff,2,max)}
        mean_payoff=mean(payoff)
      }
      if(float_strike==FALSE){
        diff_it=cp*(it_avg-basket_strike)
        payoff=diff_it*(diff_it>0)
        if(is.matrix(payoff)){payoff=apply(payoff,2,max)}
        mean_payoff=mean(payoff)
      }
      price=exp(-rq*tn)*mean_payoff
    }
    }
    return(price)
  }
