#write a function that runs EMSRb simulation n times
nEMSRbSimulation <- function(num,probs1,probs2,alpha,beta,phi1,a1,b1,a2,b2,N,BH,limits){
  k <- EMSRbSimulation(probs1,probs2,alpha,beta,phi1,a1,b1,a2,b2,N,BH,limits)
  colnames(k) <- c("A", "O", "J", "P", "R", "S", "M", "Total")
  km <- t(k)
  if (num >= 2){
    for (i in 2:num){
      k <- EMSRbSimulation(probs1,probs2,alpha,beta,phi1,a1,b1,a2,b2,N,BH,limits)
      colnames(k) <- c("A", "O", "J", "P", "R", "S", "M", "Total")
      km <- rbind(km,t(k))
    }
  }
  return(km)
}
