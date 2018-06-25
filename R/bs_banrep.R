bs_banrep<-function(ini_date, spot, mat_date, r_curve, rf_curve, vol_matrix, s_series, c_p = "call", quote_bfrr=TRUE, base=360, M=1e4, rate_type="nominal", reg_model=y ~ poly(x, 2, raw=TRUE), days_avg=20, settDays=NULL, factor=0, sigma=NULL){
  
  cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p == "C" | c_p == "Call") * 2 - 1
  days_mat=as.numeric(mat_date-ini_date)
  
  if(is.null(settDays)){
    dates_proy=ini_date+1:days_mat
  }else{
    dates_proy=settDays[(findInterval(ini_date, settDays)+1):findInterval(mat_date, settDays)]
  }
  days_proy=as.numeric(dates_proy-ini_date)
  t_proy=days_proy/base
  t_mat=tail(t_proy,1)
  r=approx(x=r_curve[,1],y=r_curve[,2],xout=days_mat)$y
  rf=approx(x=rf_curve[,1],y=rf_curve[,2],xout=days_mat)$y
  if(rate_type=="nominal"){
    r_c=log(1+r*t_mat)/t_mat
    rf_c=log(1+rf*t_mat)/t_mat
  }
  if(is.null(sigma)|is.na(sigma)){
    sigma=vol_extract(d=days_mat, strike=spot, spot=spot, r = r, rf = rf, base = base, vol_matrix=vol_matrix, quote_delta = TRUE, quote_bfrr = quote_bfrr, delta_type = 1, rate_type = rate_type, extrap = FALSE)
  }
  rates=r_c-rf_c
  s=spot_simul(spot=spot, exp_times=t_proy, rates=rates, sigma=sigma, m=M)
  N=length(days_proy)
  s_mean=matrix(0, ncol=M,nrow=N)
  s_w=s/days_avg
  s_series_w=s_series[,2]/days_avg
  s_mean[1,]=sum(tail(s_series_w,days_avg))
  for(k in 1:(N-1)){
    if(k==1){
    s_mean[k+1,]=sum(tail(s_series_w,max(0,days_avg-k)))+s_w[k,]
    }else{
    s_mean[k+1,]=sum(tail(s_series_w,max(0,days_avg-k)))+apply(s_w[max(1,k+1-days_avg):k,],2,sum)  
    }
  }
  df=exp(-r_c*c(t_proy[1],diff(t_proy)))
  
  cf_mat=matrix(rep(0,M*N),ncol=M)
  cf_mat[N,]=sapply(cp*(s[N,]-s[N-1,]),max,0)*(cp*(s[N-1,]-(1+cp*factor)*s_mean[N,])>=0)
  for(i in N:2){
    if(i==2){
      iv=sapply(cp*(s[i-1,]-tail(s_series[,2],1)),max,0)*(cp*(tail(s_series[,2],1)-(1+factor)*s_mean[i-1,])>=0)
    }else{
      iv=sapply(cp*(s[i-1,]-s[i-2,]),max,0)*(cp*(s[i-2,]-(1+factor)*s_mean[i-1,])>=0)      
    }
    itm=which(iv>0)
    if(length(itm)>0){
    y=cf_mat[i,itm]*df[i]
    x=s[i-1,itm]
    coef=lm(reg_model)
    cv=predict(coef, newdata=data.frame(x=x))
    ind_ex=iv[itm]>cv
    cf_mat[i-1,itm[ind_ex]]=iv[itm[ind_ex]]
    cf_mat[i:N,itm[ind_ex]]=0
    }
  }
  
  ##Flujos de caja descontados.
  dfs=cumprod(df)
  cf_disc=t(dfs)%*%cf_mat
  
  ##Precio:
  price=mean(cf_disc) 
  return(price)
}
