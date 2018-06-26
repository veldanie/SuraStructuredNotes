print_table_curr<-function(matrix_fx, id, days=2){
  base_curr=substr(id,1,3)
  ref_curr=substr(id,4,6)
  lid=length(id)
  spot=rep(0,lid)
  for(i in 1:length(id)){
    spot[i]=as.numeric(as.character(matrix_fx[which(base_curr[i]==colnames(matrix_fx)),which(ref_curr[i]==colnames(matrix_fx))]))
  }
  return(data.frame(ID=id, TENOR="SP", DIAS=days, SP=spot))
}
