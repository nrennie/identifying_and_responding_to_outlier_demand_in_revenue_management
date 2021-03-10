aggBH <- function(data,BH){
  k <- length(data)/BH
  if (k%%1 != 0){
    return("vector not an integer multiple of booking horizon")
  }
  else {
    output <- unname(tapply(data, (seq_along(data)-1) %/% k, sum))
    return(output)
  }
}
