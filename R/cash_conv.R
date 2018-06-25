cash_conv <-function(cash_in, curr_in, spot, spot_id){
  cash_out=NA
  if(curr_in==substr(spot_id,1,3)){
    cash_out=cash_in*spot
  }
  if(curr_in==substr(spot_id,4,6)){
    cash_out=cash_in/spot
  }
  return(cash_out)
}
