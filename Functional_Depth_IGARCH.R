garch_extrap <- function(df,k){
  return(t(apply(df,1, function (x) c(x[1:k],ceiling(as.vector((predict(garchFit(~garch(1, 1),ts(as.numeric(x[1:k]))),n.ahead=30-k)$meanForecast)))
  ))))
}

func_depth_garch_outlier <- function(df, k, maxiter=50, B=1000){
  d <- garch_extrap(df,k)
  output <- func_depth_outlier(d, maxiter=maxiter, B=B)
  return(output)
}