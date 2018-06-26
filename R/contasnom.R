contasnom=function (tasas,modori,moddes) {
  tasaini = tasas
  tasefini = tasaini/modori
  taseffin = ((1+tasefini)^(modori/moddes))-1
  tasfin = taseffin*moddes
  return(tasfin)
}
