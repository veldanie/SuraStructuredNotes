matrix_approx <-
function (data_matrix, xout, extrap=FALSE) 
{
  x_vola = data_matrix[, 1]
  y_vola = data_matrix[, -1]
  mat_names=colnames(data_matrix)
  ldv=length(mat_names)-1
  m_inter = matrix(rep(0,length(xout)* ldv), ncol = ldv )
  for (i in c(1:ldv)) {
    if(extrap==TRUE){m_inter[, i] = approx_extrap(x=x_vola, y=y_vola[,i], xout=xout)$y}
    if(extrap==FALSE){m_inter[, i] = approx(x=x_vola, y=y_vola[,i], xout=xout, yleft=y_vola[1,i], yright=tail(y_vola[,i],1))$y}
  }
  vol_out = cbind(xout, m_inter)
  colnames(vol_out) = mat_names
  return(vol_out)
}
