cir_price<-function(k, theta, sigma,lambda , Y_0, tao, c0, c1){

  if (any(length(k) != length(theta)) || any(length(k) != length(sigma)) ||any(length(k) != length(lambda))||any(length(k) != length(Y_0)))
    stop("La dimensi?n de los par?metros no coincide")

  l=length(k)
  lt=length(tao)
  alpha=2*(k*theta)/sigma^2
  gamma=((k+lambda)^2+2*sigma^2)^0.5
  A=exp(-repc(c0,lt)*repr(t(tao),l))*((2*repc(gamma,lt)*exp((gamma+k+lambda)%*%t(tao)/2))/
                                        (repc((gamma+k+lambda),lt)*(exp(gamma%*%t(tao))-1)+2*repc(gamma,lt)))^repc(alpha,lt)
  B=2*repc(c1,lt)*(exp(gamma%*%t(tao))-1)/
    (repc((gamma+k+lambda),lt)*(exp(gamma%*%t(tao))-1)+2*repc(gamma,lt))
  At=apply(A,2,prod)
  cirprice=(At)*exp(-t(B)%*%Y_0)
  return(data.frame("A"=At,"B"=t(B),"pr"=cirprice))
}
