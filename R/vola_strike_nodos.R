vola_strike_nodos <- function (spot, strikes, vola_matrix, r_quote, r_base, deltaFWD, base) {
  #Dada una matriz strikes y matriz de volatilidades, se estima una matriz delta forward de NODOS!
  vola = vola_matrix[, -1]
  days=vola_matrix[, 1]
  ldelta = length(deltaFWD)
  ldays = length(days)
  vola_obj=matrix(rep(0,ldelta*ldays),ncol=ldelta)
  for(i in c(1:ldays)){
    t=days/base
    volai=vola[1,]

    strikel1=strikes[i,2]
    striker1=strikes[i,4]
    strikel2=strikes[i,1]
    striker2=strikes[i,5]
    strikeATM=strikes[i,3]#strike del ATM

    volal1=vola[i,2]
    volar1=vola[i,4]
    volal2=vola[i,1]
    volar2=vola[i,5]
    volaATM=vola[i,3]#vola del ATM

    strike_l1=nlm(dif_strike_nodos,strikel1, spot, strikel1, strikeATM, volal1, volaATM, r_quote[i], r_base[i], t[i], 0.75)$estimate
    vola_l1=approx_extrap(c(strikel1, strikeATM), c(volal1, volaATM), strike_l1)$y

    strike_r1=nlm(dif_strike_nodos,striker1, spot, strikeATM, striker1, volaATM, volar1, r_quote[i], r_base[i], t[i], 0.25)$estimate
    vola_r1=approx_extrap(c(strikeATM, striker1), c(volaATM, volar1), strike_r1)$y

    strike_l2=nlm(dif_strike_nodos,strikel2, spot, strikel2, strike_l1, volal2, vola_l1, r_quote[i], r_base[i], t[i], 0.9)$estimate
    vola_l2=approx_extrap(c(strikel2, strike_l1), c(volal2, vola_l1), strike_l2)$y

    strike_r2=nlm(dif_strike_nodos,striker2, spot, strike_r1, striker2, vola_r1, volar2, r_quote[i], r_base[i], t[i], 0.1)$estimate
    vola_r2=approx_extrap(c(strike_r1, striker2), c(vola_r1, volar2), strike_r2)$y
    vola_obj[i,]=c(vola_l2, vola_l1, volaATM, vola_r1, vola_r2)
  }
  vola_obj=data.frame(days, vola_obj)
  colnames(vola_obj)=c("d",deltaFWD)
  return(vola_obj)
}

