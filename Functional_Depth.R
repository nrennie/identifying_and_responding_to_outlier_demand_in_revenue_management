func_depth_outlier <- function(data, maxiter=50,B=1000){
  #C <- cutoffs[ncol(data)]
  C <- functional_depth_threshold(data,B=B)
  n <- nrow(data)
  rownames(data) <- 1:n
  #array should be t (30) x n (500) x p (1)
  d <- array(t(data), dim=c(ncol(data),nrow(data),1))
  fit <- mfd(d)
  depths <- fit$MFDdepthX
  #find outliers 
  outliers <- which(depths <= C)
  i = 0
  while (length(which(depths <= C))>0 && i < maxiter){
    i = i + 1
    #remove outliers from data set
    d1 <- data[-outliers,]
    d <- array(t(d1), dim=c(ncol(d1),nrow(d1),1))
    fit <- mfd(d)
    depths <- fit$MFDdepthX
    outliers <- c(outliers, as.integer(rownames(d1[which(depths <= C),]))) 
  }
  return(sort(outliers))
}
