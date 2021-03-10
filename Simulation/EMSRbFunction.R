EMSRbFunction <- function(fares,means,vars,capacity){
  n <- length(fares)
  prolimits <- numeric(n)
  for (j in 1:(n-1)){
    m <- sum(means[1:j])
    sd <- sqrt(sum(vars[1:j]))
    if ((fares[j+1]/(sum(fares[1:j]*means[1:j])/m)) >= 1){
      if (j == 1){
        prolimits[j] = 0
      }
      else {
        prolimits[j] = prolimits[j-1]
      }
    }
    else{
      prolimits[j] <- round(min(max(0,qnorm((1 - (fares[j+1]/(sum(fares[1:j]*means[1:j])/m))), mean = m, sd = sd)),capacity))
    }
  }
  prolimits[n] <- capacity
  return(c(prolimits[1],diff(prolimits)))
}










