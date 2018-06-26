curva_est<-function(curva){

  n=length(curva$V1)
  last=curva$V1[n]
  c_est=rep(0,last)
  grad=(curva$V2[2:n]-curva$V2[1:(n-1)])/(curva$V1[2:n]-curva$V1[1:(n-1)])

  for (i in 1:(n-1)){
    x=curva$V1[i];r=curva$V2[i]
    for(j in x:(curva$V1[i+1]-1)){
      c_est[j]=r+(j-x)*grad[i]
    }
  }
  c_est[last]=curva$V2[n]

  return(c_est)
}
