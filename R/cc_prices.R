cc_prices<-function(tenor,rates){
  return(exp(-tenor*rates))
}
