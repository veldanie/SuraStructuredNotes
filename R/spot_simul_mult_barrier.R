spot_simul_mult_barrier<-function(spot, spot_ini, times, rates, sigma, rho, M = 1000, cpn_barrier=rep(0,length(spot)), ki_barrier=rep(0,length(spot)), er_barrier=rep(0,length(spot)), pos_er=NULL, denom=1){
  
  nact = length(spot)
  t_mat = times %*% t(rep(1, nact))
  SIGMA = diag(sigma) %*% rho %*% diag(sigma)
  nu = rates- sigma^2/2
  N = length(times)
  R = chol(SIGMA)
  st = array(0, dim = c(N, M, nact))
  cpn_barrier_ind=matrix(0,nrow=N,ncol=M)
  er_barrier_ind=NULL
  ki_barrier_ind=ki_lprs=rep(0,M)
  
  if(!is.null(pos_er)){
  if(length(pos_er)>1){
    er_barrier_ind=matrix(0,nrow=length(pos_er),ncol=M)
    
    for (i in 1:M) {
    x = matrix(rnorm(N * nact), ncol = nact) * sqrt(diff(c(0, times)))
    w = x %*% R
    wt = apply(w, 2, cumsum)
    si = exp(repr(nu, N) * t_mat + wt) %*% diag(spot)
    st[, i, ] = si
    cpn_barrier_ind[,i]=apply(si,1,function(x)all(x>=cpn_barrier))
    er_barrier_ind[,i]=apply(st[pos_er, i, ],1,function(x)all(x>=er_barrier))
    ki_barrier_ind[i]=all(st[length(times), i, ]>=ki_barrier)
    st_ret=round(st[length(times), i, ]/spot_ini,4)
    pos_min=which(min(st_ret)==st_ret)
    ki_lprs[i]=st[length(times),i,pos_min]*denom/ki_barrier[pos_min]
  }
  }
  if(length(pos_er)==1){
    er_barrier_ind=rep(0,M)
    for (i in 1:M) {
      x = matrix(rnorm(N * nact), ncol = nact) * sqrt(diff(c(0, times)))
      w = x %*% R
      wt = apply(w, 2, cumsum)
      si = exp(repr(nu, N) * t_mat + wt) %*% diag(spot)
      st[, i, ]=si
      cpn_barrier_ind[,i]=apply(si,1,function(x)all(x>=cpn_barrier))
      er_barrier_ind[i]=all(st[pos_er, i, ]>=er_barrier)
      ki_barrier_ind[i]=all(st[length(times), i, ]>=ki_barrier)
      st_ret=round(st[length(times), i, ]/spot_ini,4)
      pos_min=which(min(st_ret)==st_ret)
      ki_lprs[i]=st[length(times),i,pos_min]*denom/ki_barrier[pos_min]
    }
  }
  }
  if(is.null(pos_er)){
    if(length(times)==0){
      si = t(spot)
      cpn_barrier_ind=all(si>=cpn_barrier)
      ki_barrier_ind=all(si>=ki_barrier)
      st_ret=round(si/spot_ini,4)
      pos_min=which(min(st_ret)==st_ret)
      ki_lprs=si[pos_min]*denom/ki_barrier[pos_min]
      
    }
    if(length(times)>0){
    for (i in 1:M) {
      x = matrix(rnorm(N * nact), ncol = nact) * sqrt(diff(c(0, times)))
      w = x %*% R
      wt = apply(w, 2, cumsum)
      si =exp(repr(nu, N) * t_mat + wt) %*% diag(spot)
      st[, i, ]=si
      cpn_barrier_ind[,i]=apply(si,1,function(x)all(x>=cpn_barrier))
      ki_barrier_ind[i]=all(x>=ki_barrier)
      st_ret=round(st[length(times), i, ]/spot_ini,4)
      pos_min=which(min(st_ret)==st_ret)
      ki_lprs[i]=st[length(times),i,pos_min]*denom/ki_barrier[pos_min]
    }
    }
  }
  
  return(list(st=st,cpn_barrier_ind=cpn_barrier_ind,er_barrier_ind=er_barrier_ind,ki_barrier_ind=ki_barrier_ind,ki_lprs=ki_lprs))
}
 
  