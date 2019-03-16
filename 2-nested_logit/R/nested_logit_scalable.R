library(dplyr)
data <- read.delim("/home/poom/Desktop/DB833/2-nested_logit/OJ300.txt")

# Drop the columns of the dataframe
data <- select(data,c(15:31))
data <- cbind(price0 = 0, data)
model <- list()
model[[1]] <- c(0.5)
model[[2]] <- c(1,2,3,4,5,6,7,8)
choicep <- data.frame(matrix(ncol = 9, nrow = nrow(data)))

likelihood <- function(theta){
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
  lambda <- theta[10]
  
  # exp(utility)
  data$ev0 <- exp((a0 + b1*data$price0) / lambda)
  data$ev1 <- exp((a1 + b1*data$price1) / lambda)
  data$ev2 <- exp((a2 + b1*data$price2) / lambda)
  data$ev3 <- exp((a3 + b1*data$price3) / lambda)
  data$ev4 <- exp((a4 + b1*data$price4) / lambda)
  data$ev5 <- exp((a5 + b1*data$price5) / lambda)
  data$ev6 <- exp((a6 + b1*data$price6) / lambda)
  data$ev7 <- exp((a7 + b1*data$price7) / lambda)
  data$ev8 <- exp((b1*data$price8) / lambda)
  exp_util <- select(data, ev0,ev1,ev2,ev3,ev4,ev5,ev6,ev7,ev8)
  
  # choice prob
  data$all_nest_util = 0
  num_count = 1
  num_count2 = 1
  for(i in 1:length(model)){
    data$nest_j_util <- 0
    for(j in 1:length(model[[i]])){
      data$nest_j_util = data$nest_j_util + exp_util[num_count]
      choicep[, num_count] <- exp_util[num_count]
      num_count = num_count + 1
    }
    # numerator
    data$nest_j_util = data$nest_j_util^(lambda-1)
    for(j in 1:length(model[[i]])){
      choicep[, num_count2] <- choicep[, num_count2] * data$nest_j_util
      num_count2 = num_count2 +1
    }
    # denominator
    data$all_nest_util = data$all_nest_util + data$nest_j_util
    # choice prob
    choicep[, num_count-1] = choicep[, num_count-1] / data$all_nest_util
  }
  
  # log-likelihood
  data$p_chosen <- choicep[,1]*data$NP + choicep[,2]*data$br1 + choicep[,3]*data$br2 + choicep[,4]*data$br3
                + choicep[,5]*data$br4 + choicep[,6]*data$br5 + choicep[,7]*data$br6 + choicep[,8]*data$br7
                + choicep[,9]*data$br8
  data$ln_p_chosen <- log(data$p_chosen)
  return(-sum(data$ln_p_chosen)) # minimize negative log-likelihood
}

#est<-optim(c(1,2,3,4,5,6,7,8,9,0.1), likelihood, method="BFGS", hessian="TRUE") # why need BF
est<-optim(c(-0.094,-0.177,-0.082,-0.008,-0.118,-0.178,-0.055,1.730,-0.076,0.076), likelihood, method="SANN") # why need BFGS?
est
# fisher_info <- solve(est$hessian)
# prop_sigma <- sqrt(diag(fisher_info))
# prop_sigma