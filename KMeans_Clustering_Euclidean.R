kmeans_outlier <- function(data,k=2){
  if (length(unique(data)) <= k){
    return(NA)
  }
  dists <- sqrt(rowSums(data - fitted(kmeans(data, centers=k))) ^ 2)
  threshold <- (max(dists)+min(dists))/2
  output <- as.vector(which(dists >= threshold))
  return(output) 
}


