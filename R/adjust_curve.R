adjust_curve<-function(date_ini, curve_t1, curve_t0, tk_st=c("ON"), min_days=0){

  tk_m=ticket_tenor(date_ini, curve_t1[,1], tk_st=tk_st)
  if(all(curve_t1[,3]=="T")){curve_t1[,2]=curve_t0[,2]}
  if(any(curve_t1[,3]=="M")){
    pos_cp=which(curve_t1[,3]!="M" & curve_t1[,1]<=min_days)
    if(length(pos_cp)>0){curve_t1[pos_cp,2]=curve_t0[pos_cp,2]}
    xout=curve_t1[which(curve_t1[,3]!="M" & curve_t1[,1]>min_days),1]
    xout=xout[xout<max(curve_t1[which(curve_t1[,3]=="M"),1]) & xout>min(curve_t1[which(curve_t1[,3]=="M"),1])]
    if(length(xout)>0){
      pos_m=which(curve_t1[,3]=="M")
      y_var=(curve_t1[pos_m,2]-curve_t0[pos_m,2])
      ajust=approx(x=curve_t1[which(curve_t1[,3]=="M"),1], y=y_var, xout=xout)
      pos_t=match(xout, curve_t1[,1])
      curve_t1[pos_t,2]=round(curve_t0[pos_t,2]+ajust$y,5)
    }
    ##Ajuste nodos extremos.
    xout=curve_t1[which(curve_t1[,3]!="M" & curve_t1[,1]>min_days),1]
    xout_r=xout[xout>max(curve_t1[which(curve_t1[,3]=="M"),1])]
    if(length(xout_r)>0){
      pos_max_m=max(which(curve_t1[,3]=="M"))
      ajust_r=curve_t1[pos_max_m,2]-curve_t0[pos_max_m,2]
      pos_t=match(xout_r, curve_t1[,1])
      curve_t1[pos_t,2]=round(curve_t0[pos_t,2]+ajust_r,5)
    }
    xout_l=xout[xout<min(curve_t1[which(curve_t1[,3]=="M"),1])]
    if(length(xout_l)>0){
      pos_min_m=min(which(curve_t1[,3]=="M"))
      ajust_l=curve_t1[pos_min_m,2]-curve_t0[pos_min_m,2]
      pos_t=match(xout_l, curve_t1[,1])
      curve_t1[pos_t,2]=round(curve_t0[pos_t,2]+ajust_l,5)
    }
  }
  return(curve_t1)
}
