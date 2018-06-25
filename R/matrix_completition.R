matrix_completition<-function(data_matrix, x_val=NULL, spar=0.8, extrap=FALSE)
  {
  data_matrix_out=data_matrix
  if(is.null(x_val)){x_val=data_matrix_out[,1]}
  n_col=ncol(data_matrix_out)
  for(k in 1:n_col){
    pos_0=which(data_matrix_out[,k]==0);
    pos_n0=which(data_matrix_out[,k]!=0)
    if(length(pos_0)==0 | length(pos_n0)==0){next
    }else if(length(pos_n0)==1){data_matrix_out[,k]=data_matrix_out[pos_n0,k]
    }else if(length(pos_n0)>0 & length(pos_n0)<=3){
      data_matrix_out[pos_0,k]=approx_extrap(x=x_val[pos_n0],
                                            y=data_matrix_out[pos_n0,k], xout=x_val[pos_0])$y
    }else{
      interp_model=smooth.spline(x=x_val[pos_n0],y=data_matrix_out[pos_n0,k],spar=spar)
      data_matrix_out[pos_0,k]=predict(interp_model,x_val[pos_0])$y
    }  
    if(extrap==FALSE & length(pos_0)>0 & length(pos_n0)>1){
      data_matrix_out[1:pos_n0[1],k]=data_matrix_out[pos_n0[1],k]
      data_matrix_out[tail(pos_n0,1):length(x_val),k]=data_matrix_out[tail(pos_n0,1),k]
      
    }
  
  }
  col0=which(apply(data_matrix_out,2,sum)==0)
  if(length(col0)>0){data_matrix_out=data_matrix_out[,-col0]}
  return(data_matrix_out)
}

