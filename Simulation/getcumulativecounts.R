getcumulativecounts <- function(data){
  df <- t(apply(data, 1, cumsum))
  return(df)
}
