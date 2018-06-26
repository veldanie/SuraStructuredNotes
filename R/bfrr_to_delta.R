bfrr_to_delta=function(vola_matrix, header_vola){
  #vola_matrix: Matriz de BF y RR con la siguiente estructura: ATM BFi RRi ...BFj RRj, para j>i
  n_row=dim(vola_matrix)[1]
  n_col=dim(vola_matrix)[2]
  vola_delta=matrix(rep(0,n_row*(n_col-1)),nrow=n_row)

  for(i in c(1:((n_col-2)/2))){
    vola_delta[,i]=vola_matrix$ATM+vola_matrix[,2*i+1]-0.5*(vola_matrix[,2*i+2])
    vola_delta[,n_col-i]=vola_matrix$ATM+vola_matrix[,2*i+1]+0.5*(vola_matrix[,2*i+2])
  }
  vola_delta[,(i+1)]=vola_matrix$ATM;vola_delta=data.frame(vola_matrix$d,vola_delta)
  colnames(vola_delta)=header_vola
  return(vola_delta)
}
