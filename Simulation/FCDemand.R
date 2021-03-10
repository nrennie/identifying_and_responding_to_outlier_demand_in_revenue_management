FCDemand <- function(Cust,probs){
  N <- length(Cust)
  n <- length(probs)-1
  Demand <- list(A=numeric(N),O=numeric(N),J=numeric(N),P=numeric(N),R=numeric(N),S=numeric(N),M=numeric(N))
  for (i in 1:N){
    if (Cust[i] != 0){
      k <- sample(0:n, Cust[i], prob=probs, replace=TRUE)
      if (length(k) != 0){
        for (j in 1:length(k)){
          if (k[j] != 0){
            Demand[[k[j]]][i] <- Cust[i]
          }
        }
      }
    }
  }
  return(Demand)
}
