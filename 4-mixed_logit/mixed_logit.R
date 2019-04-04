library(dplyr)

data <- read.delim("/home/poom/Desktop/DB833/4-mixed_logit/Yogurt.txt")
# Drop the columns of the dataframe
data <- select(data,-c(2,3,4,5,6))
# manipulate data into each individual only purchase once
data = data[data$PanelistFirstObs == 1, ]

# norm_dist <- function(x, mean, sd){
#   return prob <- (1/sqrt(2*pi*sd^2)) * exp(-(x-mean)^2/(2*sd^2))
# }
sim_prob <- function(a1, a2, a3, mean, sd){
  b1 <- rnorm(nrow(data), mean, sd)
  #pdf1 <- dnorm(b1, mean, sd)
  b2 <- rnorm(nrow(data), mean, sd)
  
  # utility function
  v1 <- a1 + b1*data$Price1 + b2*data$Feature1
  v2 <- a2 + b1*data$Price2 + b2*data$Feature2
  v3 <- a3 + b1*data$Price3 + b2*data$Feature3
  v4 <- b1*data$Price4 + b2*data$Feature4
  
  # lni
  ev1 <- exp(v1)
  ev2 <- exp(v2)
  ev3 <- exp(v3)
  ev4 <- exp(v4)
  ev_total <- ev1 + ev2 + ev3 + ev4
  p1 <- ev1/ev_total
  p2 <- ev2/ev_total
  p3 <- ev3/ev_total
  p4 <- ev4/ev_total
  return(data.frame(p1,p2,p3,p4))
}

likelihood <- function(theta){
  # initialize parameters
  a1 <- theta[1]
  a2 <- theta[2]
  a3 <- theta[3]
  mean <- theta[4]
  sd <- theta[5]
  covar <- theta[6]
  
  # draw
  pni1 <- 0
  pni2 <- 0
  pni3 <- 0
  pni4 <- 0
  draw_num <- 100
  for (i in 1:draw_num) {
    pni_df <- sim_prob(a1, a2, a3, mean, sd)
    pni1 <- pni1 + pni_df$p1
    pni2 <- pni2 + pni_df$p2
    pni3 <- pni3 + pni_df$p3
    pni4 <- pni4 + pni_df$p4
  }
  # calculate average
  pni1 <- pni1/draw_num
  pni2 <- pni2/draw_num
  pni3 <- pni3/draw_num
  pni4 <- pni4/draw_num
  
  # log-likelihood
  p_chosen <- pni1*data$Brand1 + pni2*data$Brand2 + pni3*data$Brand3 + pni4*data$Brand4
  ln_p_chosen <- log(p_chosen)
  return(-sum(ln_p_chosen)) # minimize negative log-likelihood
}

est1<-optim(c(1,2,3,4,log(5),6), likelihood, method="BFGS", hessian="TRUE") # why need BFGS?
# est
# fisher_info <- solve(est$hessian)
# prop_sigma <- sqrt(diag(fisher_info))
# prop_sigma
