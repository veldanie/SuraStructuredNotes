opbs_pr_simul <-
function(spot, strike, drift, r, v, t, c_p = "call", n_traj=1000, rate_type="nominal") {
      #t: Time in years
      cp = (c_p == "call" | c_p == "c" | c_p == "CALL" | c_p == 
                "C" | c_p == "Call") * 2 - 1
      
      r_c = r
      if (substr(rate_type, 1, 1) == "n") {
        r_c = log(1 + r * t)/t
      }
      if (substr(rate_type, 1, 1) == "e") {
        r_c = log(1 + r)
      }
      
      St=spot*exp((drift+0.5*v)*t+v*rnorm(n_traj,sd=t))
      diff=cp*St-cp*strike
      price=exp(-r_c*t)*mean(diff*(diff>0))
      return(price)
      }
