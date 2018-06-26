vola_strike=function (spot, strike, d, r_quote, r_base, deltaFwd, data_matrix_d,base){
  volad = data_matrix_d[d, -1]
  ld = length(deltaFwd)
  imp_str = imp_cstrike(deltaFwd, spot, volad, r_quote, r_base, d/base)
  pos = max(which(strike > imp_str))
  if (pos == -Inf) {
    v = volad[1] + (imp_str[1] - strike) * (volad[1] - volad[2])/abs(imp_str[1] - imp_str[2])
  }else if (pos == ld) {v = volad[ld] + (strike - imp_str[ld]) * (volad[ld] -
                                                                    volad[ld - 1])/abs(imp_str[ld] - imp_str[ld - 1])
  }else {v = volad[pos] + (strike - imp_str[pos]) * (volad[pos + 1] - volad[pos])/(imp_str[pos + 1] - imp_str[pos])
  }
  return(v)
}
