ses_extrap <- function(df,k){
  return(t(apply(df,1, function (x) c(x[1:k],ceiling(as.vector(ses(ts(as.numeric(x[1:k])),h=30-k)$mean))))))
}

func_depth_ses_outlier <- function(df, k, maxiter=50, B=1000){
  d <- ses_extrap(df,k)
  output <- func_depth_outlier(d, maxiter=maxiter, B=B)
  return(output)
}