booking_limits_function <- function(type="EMSRb", fareprices, capacity, alpha=240, beta=1, phi1=0.5, a1=5, b1=2 ,a2=2 , b2=5 ,N=3000){
  k <- FCDemandSim(100,probs1,probs2,alpha=alpha,beta=beta,phi1=phi1,a1=a1,b1=b1,a2=a2,b2=b2,N=N)
  mean_demand <- k[[1]]
  var_demand <- k[[2]]
  if (type == "EMSRb"){
    output <- EMSRbFunction(fareprices,mean_demand,var_demand,capacity)
  }
  if (type == "EMSRbMR"){
    output <- EMSRbMRFunction(fareprices,mean_demand,var_demand,capacity)
  }
  return(output)
}


