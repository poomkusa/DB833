library(dplyr)
data <- read.delim("/home/poom/Desktop/DB833/3-latent_class/Yogurt.txt")

# Drop the columns of the dataframe
data <- select(data,c(1,7:10,15:18))

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
  ev11 <- exp(a11 + b1*mydata$Price1)
  ev21 <- exp(a21 + b1*mydata$Price2)
  ev31 <- exp(a31 + b1*mydata$Price3)
  ev41 <- exp(b1*mydata$Price4)
  
  ev_total1 <- ev11 + ev21 + ev31 + ev41
  
  p11 <- ev11/ev_total1
  p21 <- ev21/ev_total1
  p31 <- ev31/ev_total1
  p41 <- ev41/ev_total1
  
  p_chosen1 <- p11*mydata$Brand1 + p21*mydata$Brand2 + p31*mydata$Brand3 + p41*mydata$Brand4
  ll1 <- log(p_chosen1)
  
  # segment 2
  ev12 <- exp(a12 + b2*mydata$Price1)
  ev22 <- exp(a22 + b2*mydata$Price2)
  ev32 <- exp(a32 + b2*mydata$Price3)
  ev42 <- exp(b2*mydata$Price4)
  
  ev_total2 <- ev12 + ev22 + ev32 + ev42
  
  p12 <- ev12/ev_total2
  p22 <- ev22/ev_total2
  p32 <- ev32/ev_total2
  p42 <- ev42/ev_total2
  
  p_chosen2 <- p12*mydata$Brand1 + p22*mydata$Brand2 + p32*mydata$Brand3 + p42*mydata$Brand4
  ll2 <- log(p_chosen2)
  
  # sum across household
  household_sum <- data.frame(mydata$ID, ll1, ll2)
  household_sum <- aggregate(. ~ mydata.ID, data=household_sum, FUN=sum)
  # take e^ to revert to prob, then take expectation, then take log for likelihood
  prob_of_hh1 <- exp(household_sum$ll1)
  prob_of_hh2 <- exp(household_sum$ll2)
  expected_prob <- phi*prob_of_hh1 + (1-phi)*prob_of_hh2
  ll <- log(expected_prob)
  
  #write.table(-sum(household_sum$ll), file=("/home/poom/Desktop/log.txt"), append=TRUE, sep=",", col.names=FALSE, 
  #            row.names=TRUE)
  # log-likelihood
  return(-sum(ll)) # minimize negative log-likelihood
}

est<-optim(c(-0.1,0.6,-3.7,-36,4,1,-1.3,-51,0.3), likelihood, method="BFGS", mydata=data, hessian="TRUE")
est
# fisher_info <- solve(est$hessian)
# prop_sigma <- sqrt(diag(fisher_info))
# prop_sigma
