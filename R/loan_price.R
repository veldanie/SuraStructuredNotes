loan_price<-function(val_date, date_ini, date_mat, rate, rate_type, nominal, base=365, disc_price=FALSE){

  date_ant=val_date
  if(val_date>date_ini){date_ant=val_date-1}

  if(disc_price==TRUE){
    days_vector=as.numeric(c(date_mat-date_ini,date_mat-val_date,date_mat-date_ant))
    tn=days_vector/base
    if (substr(rate_type, 1, 1) == "n") {
      rate_c = log(1 + rate * tn)/tn
    }
    if (substr(tipo_tasa, 1, 1) == "e") {
      rate_c = log(1 + rate)
    }
    prices=nominal*exp(-rate_c*tn)
    price=prices[2]
    delta_price=prices[2]-prices[3]
    int_acum=prices[2]-prices[1]
  }

  if(disc_price==FALSE){
    days_vector=as.numeric(c(val_date-date_ini,date_ant-date_ini))
    tn=days_vector/base
    if (substr(rate_type, 1, 1) == "n") {
      rate_c = log(1 + rate * tn)/tn
    }
    if (substr(tipo_tasa, 1, 1) == "e") {
      rate_c = log(1 + rate)
    }
    prices=nominal*exp(rate_c*tn)
    price=prices[1]
    delta_price=prices[1]-prices[2]
    int_acum=prices[1]-nominal
  }
  return(list(price=price,delta_price=delta_price, int_acum=int_acum))
}
