dif_price<-function(params, lambda, Y_0, swp_curva, tao, swp_curva_base, tao_base, Y_0_tenor){

  if (length(swp_curva) != length(tao))
    stop("El n?mero de tasas DTF no coincide con el n?mero de vencimientos")

  if (length(swp_curva_base) != length(tao_base))
    stop("El n?mero de tasas IBR no coincide con el n?mero de vencimientos")

  l=length(lambda)
  k=params[1:l];theta=params[(l+1):(2*l)];sigma=params[(2*l+1):(3*l)]
  c0=params[(3*l+1):(4*l)];c1=params[(4*l+1):(5*l)]

  ls=length(swp_curva)
  lb=length(swp_curva_base)
  swp_base2=rep(0,lb)
  for (i in 1:ls){
    swp_base2=swp_base2+swp_curva_base*(tao_base==tao[i])
  }
  swp_base2=swp_base2[swp_base2!=0]

  mkt_prices=cc_prices(tao,swp_curva)
  mkt_prices_base=cc_prices(tao,swp_base2)

  float_price=cc_prices(Y_0_tenor, Y_0)
  cirprices=cir_price(k, theta, sigma, lambda, Y_0, tao-Y_0_tenor, c0, c1)
  est_prices=(float_price*cirprices$pr)*mkt_prices_base

  dif=est_prices-mkt_prices
  dif_sqrt=t(dif)%*%dif
  return(dif_sqrt)
}
