library(dplyr)
data <- read.delim("/home/poom/Desktop/DB833/3-latent_class/Yogurt.txt")

# Drop the columns of the dataframe
data <- select(data,c(1,7:10,15:19))

# a11 <- 1
# a21 <- 2
# a31 <- 3
# b1 <- 4
# a12 <- 5
# a22 <- 6
# a32 <- 7
# b2 <- 8
# rho <- 9
# phi <- exp(rho)/(1+exp(rho))
# # segment 1
# data$ev11 <- exp(a11 + b1*data$Price1)
# data$ev21 <- exp(a21 + b1*data$Price2)
# data$ev31 <- exp(a31 + b1*data$Price3)
# data$ev41 <- exp(b1*data$Price4)
# 
# data$ev_total1 <- data$ev11 + data$ev21 + data$ev31 + data$ev41
# 
# data$p11 <- data$ev11/data$ev_total1
# data$p21 <- data$ev21/data$ev_total1
# data$p31 <- data$ev31/data$ev_total1
# data$p41 <- data$ev41/data$ev_total1
# 
# data$p_chosen1 <- data$p11*data$Brand1 + data$p21*data$Brand2 + data$p31*data$Brand3 + data$p41*data$Brand4
# data$ll1 <- log(data$p_chosen1)
# 
# # segment 2
# data$ev12 <- exp(a12 + b2*data$Price1)
# data$ev22 <- exp(a22 + b2*data$Price2)
# data$ev32 <- exp(a32 + b2*data$Price3)
# data$ev42 <- exp(b2*data$Price4)
# 
# data$ev_total2 <- data$ev12 + data$ev22 + data$ev32 + data$ev42
# 
# data$p12 <- data$ev12/data$ev_total2
# data$p22 <- data$ev22/data$ev_total2
# data$p32 <- data$ev32/data$ev_total2
# data$p42 <- data$ev42/data$ev_total2
# 
# data$p_chosen2 <- data$p12*data$Brand1 + data$p22*data$Brand2 + data$p32*data$Brand3 + data$p42*data$Brand4
# data$ll2 <- log(data$p_chosen2)
# data$ll1h <- 0
# data$ll2h <- 0
# # sum across household
# for (i in min(data$ID):max(data$ID)){
#   data$ll1h <- data$ll1h + ifelse(data$ID == i & data$PanelistFirstObs == 1, sum(data[data$ID == i,]$ll1), 0)
#   data$ll2h <- data$ll2h + ifelse(data$ID == i & data$PanelistFirstObs == 1, sum(data[data$ID == i,]$ll2), 0)
# }
# # take e^ to revert to prob, then take expectation, then take log for likelihood
# data$prob_of_hh1 <- exp(data$ll1h)
# data$prob_of_hh2 <- exp(data$ll2h)
# data$expected_prob <- phi*data$prob_of_hh1 + (1-phi)*data$prob_of_hh2
# data$ll <- log(data$expected_prob)

likelihood <- function(theta, mydata){
  # initialize parameters
  a11 <- theta[1]
  a21 <- theta[2]
  a31 <- theta[3]
  b1 <- theta[4]
  
  a12 <- theta[5]
  a22 <- theta[6]
  a32 <- theta[7]
  b2 <- theta[8]
  
  rho <- theta[9]
  phi <- exp(rho)/(1+exp(rho))
  
  # segment 1
  data$ev11 <- exp(a11 + b1*data$Price1)
  data$ev21 <- exp(a21 + b1*data$Price2)
  data$ev31 <- exp(a31 + b1*data$Price3)
  data$ev41 <- exp(b1*data$Price4)
  
  data$ev_total1 <- data$ev11 + data$ev21 + data$ev31 + data$ev41
  
  data$p11 <- data$ev11/data$ev_total1
  data$p21 <- data$ev21/data$ev_total1
  data$p31 <- data$ev31/data$ev_total1
  data$p41 <- data$ev41/data$ev_total1
  
  data$p_chosen1 <- data$p11*data$Brand1 + data$p21*data$Brand2 + data$p31*data$Brand3 + data$p41*data$Brand4
  data$ll1 <- log(data$p_chosen1)
  
  # segment 2
  data$ev12 <- exp(a12 + b2*data$Price1)
  data$ev22 <- exp(a22 + b2*data$Price2)
  data$ev32 <- exp(a32 + b2*data$Price3)
  data$ev42 <- exp(b2*data$Price4)
  
  data$ev_total2 <- data$ev12 + data$ev22 + data$ev32 + data$ev42
  
  data$p12 <- data$ev12/data$ev_total2
  data$p22 <- data$ev22/data$ev_total2
  data$p32 <- data$ev32/data$ev_total2
  data$p42 <- data$ev42/data$ev_total2
  
  data$p_chosen2 <- data$p12*data$Brand1 + data$p22*data$Brand2 + data$p32*data$Brand3 + data$p42*data$Brand4
  data$ll2 <- log(data$p_chosen2)
  data$ll1h <- 0
  data$ll2h <- 0
  # sum across household
  for (i in min(data$ID):max(data$ID)){
    data$ll1h <- data$ll1h + ifelse(data$ID == i & data$PanelistFirstObs == 1, sum(data[data$ID == i,]$ll1), 0)
    data$ll2h <- data$ll2h + ifelse(data$ID == i & data$PanelistFirstObs == 1, sum(data[data$ID == i,]$ll2), 0)
  }
  # take e^ to revert to prob, then take expectation, then take log for likelihood
  data$prob_of_hh1 <- exp(data$ll1h)
  data$prob_of_hh2 <- exp(data$ll2h)
  data$expected_prob <- phi*data$prob_of_hh1 + (1-phi)*data$prob_of_hh2
  data$ll <- log(data$expected_prob)
  
  #write.table(-sum(household_sum$ll), file=("/home/poom/Desktop/log.txt"), append=TRUE, sep=",", col.names=FALSE, 
  #            row.names=TRUE)
  # log-likelihood
  return(-sum(data$ll)) # minimize negative log-likelihood
}

est<-optim(c(-0.1,0.6,-3.7,-36,4,1,-1.3,-51,0.3), likelihood, method="BFGS", mydata=data, hessian="TRUE")
est
fisher_info <- solve(est$hessian)
prop_sigma <- sqrt(diag(fisher_info))
prop_sigma
