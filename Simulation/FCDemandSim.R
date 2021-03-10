FCDemandSim <- function(n,probs1,probs2,alpha,beta,phi1,a1,b1,a2,b2,N){  
  Cust <- NHPP(alpha,beta,phi1,a1,b1,a2,b2,N)
  Cust1 <- Cust[[1]]
  Cust2 <- Cust[[2]]
  Ademand <- numeric(n)
  Odemand <- numeric(n)
  Jdemand <- numeric(n)
  Pdemand <- numeric(n)
  Rdemand <- numeric(n)
  Sdemand <- numeric(n)
  Mdemand <- numeric(n)
  for (i in 1:n){
    D1 <- FCDemand(Cust1,probs1)
    D2 <- FCDemand(Cust2,probs2)
    Demand <- list(A=D1$A+D2$A,O=D1$O+D2$O,J=D1$J+D2$J,P=D1$P+D2$P,R=D1$R+D2$R,S=D1$S+D2$S,M=D1$M+D2$M)
    Ademand[i] <- sum(Demand$A)
    Odemand[i] <- sum(Demand$O)
    Jdemand[i] <- sum(Demand$J)
    Pdemand[i] <- sum(Demand$P)
    Rdemand[i] <- sum(Demand$R)
    Sdemand[i] <- sum(Demand$S)
    Mdemand[i] <- sum(Demand$M)
  }
  out <- list(c(mean(Ademand),mean(Odemand),mean(Jdemand),mean(Pdemand),mean(Rdemand),mean(Sdemand),mean(Mdemand)),c(var(Ademand),var(Odemand),var(Jdemand),var(Pdemand),var(Rdemand),var(Sdemand),var(Mdemand)))
  return(out)
}
