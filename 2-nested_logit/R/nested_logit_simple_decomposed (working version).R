library(dplyr)
data <- read.delim("/home/poom/Downloads/OJ300.txt")

# Drop unnecessary columns of the dataframe
data <- select(data,c(15:31))
data <- cbind(price0 = 0, data)

# checking
# summary(data)
# which(is.na(data$price1))
data <- data[complete.cases(data), ]
# cor(select(data,c(1:9)), use="complete")

likelihood <- function(theta, data){
  # initialize parameters
  a0 <- theta[1]
  a1 <- theta[2]
  a2 <- theta[3]
  a3 <- theta[4]
  a4 <- theta[5]
  a5 <- theta[6]
  a6 <- theta[7]
  a7 <- theta[8]
  b1 <- theta[9]
  lambda1 <- 1
  lambda2 <- theta[10]
  rho <- exp(lambda2)/(1+exp(lambda2))
  
  
  # exp(utility)
  ev0 <- exp((a0) / lambda1)
  ev1 <- exp((a1 + b1*data$price1) / rho)
  ev2 <- exp((a2 + b1*data$price2) / rho)
  ev3 <- exp((a3 + b1*data$price3) / rho)
  ev4 <- exp((a4 + b1*data$price4) / rho)
  ev5 <- exp((a5 + b1*data$price5) / rho)
  ev6 <- exp((a6 + b1*data$price6) / rho)
  ev7 <- exp((a7 + b1*data$price7) / rho)
  ev8 <- exp((b1*data$price8) / rho)
  
  etotal_nest1 <- ev0
  etotal_nest2 <- ev1+ev2+ev3+ev4+ev5+ev6+ev7+ev8
  iv1 <- log(etotal_nest1)
  iv2 <- log(etotal_nest2)
  pnest1 <- ev0/(exp(rho*iv2)+ev0)     # numerator = e^(ln(e^(a0))) = e^a0 = ev0
  pnest2 <- exp(rho*iv2)/(exp(rho*iv2)+ev0)
  
  # choice prob
  p0 = ev0/(exp(rho*iv2)+ev0)     #pnest1 b/c conditional prob is 1
  #p1 = pnest2*(ev1/etotal_nest2)  #with bracket, give slightly different result
  p1 = pnest2*ev1/etotal_nest2
  p2 = pnest2*ev2/etotal_nest2
  p3 = pnest2*ev3/etotal_nest2
  p4 = pnest2*ev4/etotal_nest2
  p5 = pnest2*ev5/etotal_nest2
  p6 = pnest2*ev6/etotal_nest2
  p7 = pnest2*ev7/etotal_nest2
  p8 = pnest2*ev8/etotal_nest2
  
  # using loop gives slightly different result
  # LL <- 0
  # N <- nrow(data)
  # 
  # for (i in 1:N){
  #   indL <- 0
  #   if (data$br1[i] == 1){
  #     indL <- p1[i]
  #   }
  #   else if (data$br2[i] == 1){
  #     indL <- p2[i]
  #   }
  #   else if (data$br3[i] == 1){
  #     indL <- p3[i]
  #   }
  #   else if (data$br4[i] == 1){
  #     indL <- p4[i]
  #   }
  #   else if (data$br5[i] == 1){
  #     indL <- p5[i]
  #   }
  #   else if (data$br6[i] == 1){
  #     indL <- p6[i]
  #   }
  #   else if (data$br7[i] == 1){
  #     indL <- p7[i]
  #   }
  #   else if (data$br8[i] == 1){
  #     indL <- p8[i]
  #   }
  #   else if (data$NP[i] == 1){
  #     indL <- p0[i]
  #   }
  # 
  #   indLL <- log(indL)
  # 
  #   LL <- LL+indLL
  # }
  # return(-LL)
  
  # prob of chosen choice
  p_chosen = p0*data$NP + p1*data$br1 + p2*data$br2 + p3*data$br3 + p4*data$br4 +
    p5*data$br5 + p6*data$br6 + p7*data$br7 + p8*data$br8

  # log likelihood
  ln_p_chosen <- log(p_chosen)
  return(-sum(ln_p_chosen)) # minimize negative log-likelihood
}

est<-optim(c(0,0,0,0,0,0,0,0,-1,0.1), likelihood, method="BFGS", data=data, hessian = TRUE)
est
# fisher_info <- solve(est$hessian)
# prop_sigma <- sqrt(diag(fisher_info))
# prop_sigma