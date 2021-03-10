nhpprate <- function(A,a,b,phi,t,N){
  l <- A*phi*dbeta(t/N,a,b)
  return(l)
}
