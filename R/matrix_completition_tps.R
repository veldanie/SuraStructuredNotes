matrix_completition_tps<-function(vol_matrix, extrap=TRUE, method="GCV", replace=FALSE, plot_surface=FALSE){
  pos_n0=which(vol_matrix[,-1]!=0)
  dim_mat=dim(vol_matrix[,-1])
  seqr=seq(0,prod(dim_mat),dim_mat[1])
  pos_col=findInterval(pos_n0,seqr)
  pos_row=pos_n0-seqr[pos_col]; 
  
  pos_col[which(pos_row==0)]=pos_col[which(pos_row==0)-1]
  pos_row[pos_row==0]=dim_mat[1]
  
  strikes=as.numeric(colnames(vol_matrix)[-1])
  days=as.numeric(vol_matrix[,1])
  x=cbind(days=days[pos_row], str=strikes[pos_col])
  Y=as.vector(vol_matrix[,-1])[pos_n0]
  
  surface=Tps(x, Y)
  pred=predictSurface(surface,grid.list=list(days=days,str=strikes), extrap=extrap)
  mat_pred=pred$z
  colnames(mat_pred)=strikes
  
  if (replace==FALSE){
    mat_pred[cbind(pos_row,pos_col)]=Y
  }
  vol_matrix_tps=data.frame(d=days, mat_pred, check.names=FALSE)
  if(plot_surface==TRUE){plot.surface(pred, type="p")}

  return(vol_matrix_tps)
}