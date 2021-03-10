EMSRbMRFunction <- function(fares,means,vars,capacity){
  n <- length(fares)
  #calculate marginal revenue
  MR <- numeric(length=n)
  MR[1] <- fares[1]
  for (i in 2:n){
    MR[i] <- ((fares*cumsum(means))[i] - (fares*cumsum(means))[i-1])/(cumsum(means)[i]-cumsum(means)[i-1])
  }
  #check which fares are negative
  negmr <- which(MR < 0)
  fareclasses <- 1:n
  if (length(negmr) > 0){
  availfares <- fareclasses[-negmr]
  #aggregate demand
  for (i in 1:length(negmr)){
    if (negmr[i] != n){
      means[negmr[i] + 1] <- means[negmr[i] + 1] + means[negmr[i]]
      vars[negmr[i] + 1] <- vars[negmr[i] + 1] + vars[negmr[i]]
    }
  }
  m <- means[availfares]
  v <- vars[availfares]
  #calculate EMSRb limits for those with positive MR
  prolimits <- EMSRbFunction(fares=MR[availfares],means=m,vars=v,capacity=capacity) 
  #add in closure limits
  for (i in 1:length(negmr)){
    prolimits <- insert(prolimits,negmr[i],0)
  }
  }
  else {
    prolimits <- EMSRbFunction(fares=MR,means=means,vars=vars,capacity=capacity)
  }
  #return number of seats reserved for each fare class
  return(prolimits)
}





