poissonparametrictolerancelevel <- function(data, coverage, confidence){
  k <- poistol.int(x = data, n=length(data), m=1, alpha = 1 - confidence, P = coverage, side = 2, method="LS")
  return(list(l=k$`2-sided.lower`,u=min(k$`2-sided.upper`,200)))  
}

ptolintsoutlier <- function(data, coverage=0.95, confidence=0.95){
  k <- poissonparametrictolerancelevel(data, coverage=0.95, confidence=0.95)
  return(sort(c(which(data < k$l), which(data > k$u))))
}

