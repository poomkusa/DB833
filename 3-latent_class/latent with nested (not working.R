p_chosen_nl <- function(theta,mydata){
  a1 <- theta[1]
  a2 <- theta[2]
  a3 <- theta[3]
  a4 <- theta[4]
  a5 <- theta[5]
  a6 <- theta[6]
  a7 <- theta[7]
  an <- theta[8]
  beta <- theta[9]
  delta <- theta[10]
  rho <- exp(delta)/(1+exp(delta))
  e1 <- exp((a1+beta*mydata$price1)/rho)
  e2 <- exp((a2+beta*mydata$price2)/rho)
  e3 <- exp((a3+beta*mydata$price3)/rho)
  e4 <- exp((a4+beta*mydata$price4)/rho)
  e5 <- exp((a5+beta*mydata$price5)/rho)
  e6 <- exp((a6+beta*mydata$price6)/rho)
  e7 <- exp((a7+beta*mydata$price7)/rho)
  e8 <- exp((beta*mydata$price8)/rho)
  en <- exp(an)
  etotal <- e1+e2+e3+e4+e5+e6+e7+e8
  IV <- log(etotal)
  Pnest <- exp(rho*IV)/(exp(rho*IV)+en)
  
  P1 <- Pnest*e1/etotal	
  P2 <- Pnest*e2/etotal	
  P3 <- Pnest*e3/etotal	
  P4 <- Pnest*e4/etotal	
  P5 <- Pnest*e5/etotal	
  P6 <- Pnest*e6/etotal	
  P7 <- Pnest*e7/etotal	
  P8 <- Pnest*e8/etotal	
  Pn <- en/(exp(rho*IV)+en)
  pchosen <- P1*mydata$br1 + P2*mydata$br2 + P3*mydata$br3 + P4*mydata$br4 + P5*mydata$br5 + 
    P6*mydata$br6 + P7*mydata$br7 + P8*mydata$br8 + Pn*mydata$NP 
  return(pchosen)
}

library(dplyr)
data <- read.delim("/home/poom/Desktop/OJ300.txt")

# Drop the columns of the dataframe
data <- select(data,c(1,7:10,15:18))

likelihood <- function(theta, mydata){
  # initialize parameters
  
  a11 <- theta[1]
  a21 <- theta[2]
  a31 <- theta[3]
  a41 <- theta[4]
  a51 <- theta[5]
  a61 <- theta[6]
  a71 <- theta[7]
  an1 <- theta[8]
  b1 <- theta[9]
  delta1 <- theta[10]
  
  a12 <- theta[11]
  a22 <- theta[12]
  a32 <- theta[13]
  a42 <- theta[14]
  a52 <- theta[15]
  a62 <- theta[16]
  a72 <- theta[17]
  an2 <- theta[18]
  b2 <- theta[19]
  delta2 <- theta[20]
  
  rho <- theta[21]
  phi <- exp(rho)/(1+exp(rho))
  
  # segment 1
  mydata$p_chosen1 <- p_chosen_nl(c(a11,a21,a31,a41,a51,a61,a71,an1,b1,delta1),mydata)
  mydata$ll1 <- log(data$p_chosen1)
  
  # segment 2
  mydata$p_chosen2 <- p_chosen_nl(c(a12,a22,a32,a42,a52,a62,a72,an2,b2,delta2),mydata)
  mydata$ll2 <- log(data$p_chosen2)
  
  # sum across household
  household_sum <- select(mydata,c('ID','ll1','ll2'))
  household_sum <- aggregate(. ~ ID, data=household_sum, FUN=sum)
  # take e^ to revert to prob, then take expectation, then take log for likelihood
  household_sum$prob_of_hh1 <- exp(household_sum$ll1)
  household_sum$prob_of_hh2 <- exp(household_sum$ll2)
  household_sum$expected_prob <- phi*household_sum$prob_of_hh1 + (1-phi)*household_sum$prob_of_hh2
  household_sum$ll <- log(household_sum$expected_prob)
  
  #write.table(-sum(household_sum$ll), file=("/home/poom/Desktop/log.txt"), append=TRUE, sep=",", col.names=FALSE, 
  #            row.names=TRUE)
  # log-likelihood
  return(-sum(household_sum$ll)) # minimize negative log-likelihood
}

est<-optim(rnorm(21), likelihood, method="BFGS", mydata=data, hessian="TRUE")
est
fisher_info <- solve(est$hessian)
prop_sigma <- sqrt(diag(fisher_info))
prop_sigma
