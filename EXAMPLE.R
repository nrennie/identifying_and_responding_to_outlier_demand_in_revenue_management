#load functions
source("Tolerance_Intervals_Parametric.R")
source("KMeans_Clustering_Euclidean.R")
source("Functional_Depth.R")
source("Functional_Depth_Threshold.R")
source("Functional_Depth_ARIMA.R")
source("Functional_Depth_IGARCH.R")
source("Functional_Depth_SES.R")

#load packages
source("required_packages.R")

#load data file 
example_data <- readRDS("example_data.rds")

#run on complete observations
ptolintsoutlier(example_data[,30], coverage=0.95, confidence=0.95)
kmeans_outlier(example_data,k=2)
func_depth_outlier(example_data,maxiter=50,B=1000)

#partial data file (up to time k=15)
example_data_k15 <- example_data[,1:15]
func_depth_outlier(example_data_k15,maxiter=50,B=1000)
func_depth_arima_outlier(example_data_k15,k=15,maxiter=50,B=1000)
func_depth_garch_outlier(example_data_k15,k=15,maxiter=50,B=1000)
func_depth_ses_outlier(example_data_k15,k=15,maxiter=50,B=1000)




