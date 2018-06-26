difopt_price<-function(params, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf, spot, bs_str, t, bs_pr){
  #strike, r, rf y t son vectores.
  if (dim(bs_str)[1] != dim(bs_str)[1])
    stop("La dimensi?n de la matriz de strikes no coincide con la dimesi?n de la matriz de precios")

  if (dim(bs_str)[2] != dim(bs_str)[2])
    stop("La dimensi?n de la matriz de strikes no coincide con la dimesi?n de la matriz de precios")

  lp=length(params)

  dmat=dim(bs_str);lt=dmat[1];ld=dmat[2]
  kv=params[1];thetav=params[2];sigmav=params[3]
  muj=params[4];sigmaj=params[5]
  lambda0=params[6];lambda1=params[7];p=params[8]
  bates_pr=matrix(rep(0,lt*ld),nrow=lt)

  for(j in 1:ld){
    for (i in 1:lt){
      bates_pr[i,j]=callbates_pr(kv, thetav, sigmav, muj, sigmaj, lambda0, lambda1, p, kr, thetar, sigmar,kf, thetaf, sigmaf, var, r, rf, spot, bs_str[i,j], t[i])
    }}
  dif_pr=sum((bs_pr/spot-bates_pr/spot)^2)
  return(dif_pr)
}
