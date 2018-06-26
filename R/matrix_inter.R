matrix_inter <- function(data_matrix){
  ltv=dim(data_matrix)[1];ldv=dim(data_matrix)[2]
  tvola=data_matrix[,1];last=tail(tvola,1)
  m_inter=matrix(rep(0, last*(ldv-1)), nrow=last, ncol=ldv-1)
  if(tvola[1]!=1){
    data_matrix1=data_matrix[1,-1]-(tvola[1]-1)*(data_matrix[1,-1]-data_matrix[2,-1])/(tvola[1]-tvola[2])
    vola_matrix=rbind(data_matrix1,data_matrix[,-1])
    tvola=c(1,tvola)
  }else{vola_matrix=data_matrix[,-1]}
  for(i in c(1:(ldv-1))){
    curva_vola=data.frame(V1=tvola, V2=vola_matrix[,i])
    m_inter[,i]=curva_est(curva_vola)
  }
  new_matrix=cbind(c(1:last),m_inter)
  colnames(new_matrix)=colnames(data_matrix)
  return(new_matrix)
}

