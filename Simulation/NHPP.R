NHPP <- function(alpha,beta,phi1,a1,b1,a2,b2,N){
  A <- rgamma(1,shape=alpha,rate=beta)/30
  phi2 <- 1-phi1
  Cust1 <- numeric(N)
  Cust2 <- numeric(N)
  for (t in 1:N){
    l1 <- nhpprate(A,a1,b1,phi1,t,N)*0.01
    Cust1[t] <- rpois(1,l1)
    l2 <- nhpprate(A,a2,b2,phi2,t,N)*0.01
    Cust2[t] <- rpois(1,l2)
  }
  return(list(Cust1,Cust2))
}
