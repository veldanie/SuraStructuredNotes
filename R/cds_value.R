cds_value <-
function(cds, days_mat, hazard_curve, days_freqcds, rates, rec=0.4, base=360, Prot_Buyer=1){
        #days_haz: Attachment points for the piecewise constant hazard rate function
        #rates: Yield Curve. Interpolated. Daily rates. Nominal rates
        days_haz=hazard_curve[,1]
        hazard=hazard_curve[,2]
        days_freqcds_mat=days_freqcds[days_freqcds<=days_mat]
        hazard_freq=rep(0, length(days_freqcds_mat))
        pos_freq=match(days_haz, days_freqcds_mat)
        hazard_freq[pos_freq]=hazard
        hazard_freq[1:pos_freq[1]]=hazard[1];j=1
        if(length(pos_freq)>1){
          for(i in c(1:(length(pos_freq)-1))){
            hazard_freq[(pos_freq[i]+1):pos_freq[i+1]]=hazard[i+1]
          }}
        t=days_haz/base
        dt=(days_freqcds_mat-c(0,days_freqcds_mat[1:(length(days_freqcds_mat)-1)]))/base
        t_freqcds=days_freqcds_mat/base
        haz=hazard_cum(t_freqcds, hazard_freq)
        
        df=1/(1+rates[days_freqcds_mat]*days_freqcds_mat/base)
        
        if(prot_buyer==1){b=1}else{b=-1}
        prem_leg=cds*sum(exp(-haz)*df*dt)
        def_leg=(1-rec)*sum(hazard_freq*exp(-haz)*df*dt)
        return((def_leg-prem_leg)*b)
      }
