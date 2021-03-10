#load functions
source("NHPP.R")
source("nhpprate.R")
source("aggBH.R")
source("getcumulativecounts.R")
source("FCDemand.R")
source("FCDemandSim.R")
source("EMSRbFunction.R")
source("EMSRbMRFunction.R")
source("booking_limits_function.R")
source("EMSRbSimulation.R")
source("nEMSRbSimulation.R")


#load parameters
source("parameters.R")

#load packages
source("required_packages.R")

#calculate booking limits
pl <- booking_limits_function(type="EMSRb", fareprices, capacity)
PL <- rev(cumsum(rev(pl)))
plmr <- booking_limits_function(type="EMSRbMR", fareprices, capacity)
PLMR <- rev(cumsum(rev(plmr)))

#generate bookings
set.seed(123)
normal_data <- nEMSRbSimulation(num=475,probs1,probs2,alpha=240,beta=1,phi1=0.5,a1=5,b1=2,a2=2,b2=5,N=3000,BH=30,limits=PL)
outlier_data <- nEMSRbSimulation(num=25,probs1,probs2,alpha=240,beta=1,phi1=0.5,a1=5,b1=2,a2=2,b2=5,N=3000,BH=30,limits=PL)
normal_data <- normal_data[seq(8, nrow(normal_data), 8), ]
outlier_data <- outlier_data[seq(8, nrow(outlier_data), 8), ]
example_data <- getcumulativecounts(rbind(normal_data, outlier_data))

saveRDS(example_data, "example_data.rds")
