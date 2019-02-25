#library(dplyr)
data <- read.delim("/home/poom/Desktop/Untitled Folder/Yogurt.txt")

# Drop the columns of the dataframe
#data <- select(data,-c(1,2,3,4,5,6,19))

likelihood <- function(theta){
  # initialize parameters
  a1 <- theta[1]
  a2 <- theta[2]
  a3 <- theta[3]
  b1 <- theta[4]
  b2 <- theta[5]
  
  # utility function
  data$v1 <- a1 + b1*data$Price1 + b2*data$Feature1
  data$v2 <- a2 + b1*data$Price2 + b2*data$Feature2
  data$v3 <- a3 + b1*data$Price3 + b2*data$Feature3
  data$v4 <- b1*data$Price4 + b2*data$Feature4
  
  # logit prob
  data$ev1 <- exp(data$v1)
  data$ev2 <- exp(data$v2)
  data$ev3 <- exp(data$v3)
  data$ev4 <- exp(data$v4)
  data$ev_total <- data$ev1 + data$ev2 + data$ev3 + data$ev4
  data$p1 <- data$ev1/data$ev_total
  data$p2 <- data$ev2/data$ev_total
  data$p3 <- data$ev3/data$ev_total
  data$p4 <- data$ev4/data$ev_total
  
  # log-likelihood
  data$p_chosen <- data$p1*data$Brand1 + data$p2*data$Brand2 + data$p3*data$Brand3 + data$p4*data$Brand4
  data$ln_p_chosen <- log(data$p_chosen)
  return(-sum(data$ln_p_chosen)) # minimize negative log-likelihood
}

est<-optim(c(1,2,3,4,5), likelihood, method="BFGS", hessian="TRUE") # why need BFGS?
est
fisher_info <- solve(est$hessian)
prop_sigma <- sqrt(diag(fisher_info))
prop_sigma
