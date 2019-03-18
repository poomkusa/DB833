library(dplyr)
data <- read.delim("/home/phd-01/Desktop/DB833/2-nested_logit/OJ300.txt")

# Drop unnecessary columns of the dataframe
data <- select(data,c(15:31))
data <- cbind(price0 = 0, data)

# checking
summary(data)
which(is.na(data$price1))
data <- data[complete.cases(data), ]
cor(select(data,c(1:9)), use="complete")

likelihood <- function(theta){
  # initialize parameters
  a0 <- theta[1]
  a1 <- theta[2]
  a2 <- theta[3]
  a3 <- theta[4]
  a4 <- theta[5]
  a5 <- theta[6]
  a6 <- theta[7]
  #a7 <- theta[8]
  b1 <- theta[8]
  lambda1 <- 1
  lambda2 <- theta[9]
  
  # exp(utility)
  data$ev0 <- exp(0)
  data$ev1 <- exp((a1 + b1*data$price1) / lambda2)
  data$ev2 <- exp((a2 + b1*data$price2) / lambda2)
  data$ev3 <- exp((a3 + b1*data$price3) / lambda2)
  data$ev4 <- exp((a4 + b1*data$price4) / lambda2)
  data$ev5 <- exp((a5 + b1*data$price5) / lambda2)
  data$ev6 <- exp((a6 + b1*data$price6) / lambda2)
  data$ev7 <- exp((b1*data$price7) / lambda2)
  data$ev8 <- exp((a0 + b1*data$price8) / lambda2)
  
  # total utility for each nest and all nest
  data$nest1_util <- data$ev0^(lambda1-1)
  data$nest2_util <- (data$ev1 + data$ev2 + data$ev3 + data$ev4 + data$ev5 + data$ev6 + data$ev7 + data$ev8)^(lambda2-1)
  data$total_util <- data$nest1_util + data$nest2_util
  
  # choice prob
  data$p0 = (data$ev0 * data$nest1_util) / data$total_util
  data$p1 = (data$ev1 * data$nest2_util) / data$total_util
  data$p2 = (data$ev2 * data$nest2_util) / data$total_util
  data$p3 = (data$ev3 * data$nest2_util) / data$total_util
  data$p4 = (data$ev4 * data$nest2_util) / data$total_util
  data$p5 = (data$ev5 * data$nest2_util) / data$total_util
  data$p6 = (data$ev6 * data$nest2_util) / data$total_util
  data$p7 = (data$ev7 * data$nest2_util) / data$total_util
  data$p8 = (data$ev8 * data$nest2_util) / data$total_util
  
  # prob of chosen choice
  data$p_chosen = data$p0*data$NP + data$p1*data$br1 + data$p2*data$br2 + data$p3*data$br3 + data$p4*data$br4 + 
    data$p5*data$br5 + data$p6*data$br6 + data$p7*data$br7 + data$p8*data$br8
  
  # log likelihood
  data$ln_p_chosen <- log(data$p_chosen)
  return(-sum(data$ln_p_chosen)) # minimize negative log-likelihood
}

est<-optim(c(-0.094,-0.177,-0.082,-0.008,-0.118,-0.178,1.730,-0.076,0.076), likelihood, 
           method="BFGS", hessian = TRUE)
est
fisher_info <- solve(est$hessian)
prop_sigma <- sqrt(diag(fisher_info))
prop_sigma