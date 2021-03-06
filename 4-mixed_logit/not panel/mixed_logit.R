library(dplyr)
library(mvtnorm)

data <- read.delim("/home/poom/Desktop/DB833/4-mixed_logit/Yogurt.txt")
# Drop the columns of the dataframe
data <- select(data,-c(2,3,4,5,6))
# manipulate data into each individual only purchase once
data = data[data$PanelistFirstObs == 1, ]

# norm_dist <- function(x, mean, sd){
#   return prob <- (1/sqrt(2*pi*sd^2)) * exp(-(x-mean)^2/(2*sd^2))
# }
sim_prob <- function(a1, a2, a3, mu_vec, sigma_mtx){
  # draw from mult norm dist
  #pdf1 <- dnorm(b1, mean, sd)
  b <- rmvnorm(n=nrow(data), mean=mu_vec, sigma=sigma_mtx, method="chol")
  b1 <- b[,1]
  b2 <- b[,2]
  
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
  mu_vec <- c(theta[4], theta[5])
  sigma_b1 <- exp(theta[6])
  sigma_b1 <- exp(theta[7])
  cov_b1b2 <- theta[8]
  sigma_mtx <- matrix(c(sigma_b1,cov_b1b2,cov_b1b2,sigma_b2), ncol=2)
  #simple parameters for testing
  # mu_vec <- c(theta[4], theta[4])
  # sigma_b1 <- exp(theta[5])
  # sigma_mtx <- matrix(c(sigma_b1,0,0,sigma_b1), ncol=2)
  
  # normalize covariance matrix (for probit, not mixed-logit)
  # ident <- diag(1)
  # M <- cbind(ident[,-ncol(ident)], -1, ident[,ncol(ident)])
  # omega_tilde <- M %*% sigma_mtx %*% t(M)
  # omega_tilde <- omega_tilde/omega_tilde[1,1]
  
  # simulation
  pni1 <- 0
  pni2 <- 0
  pni3 <- 0
  pni4 <- 0
  draw_num <- 100
  for (i in 1:draw_num) {
    pni_df <- sim_prob(a1, a2, a3, mu_vec, sigma_mtx)
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

mu_b1 <- 1
mu_b2 <- 2
sigma_b1 <- 3
sigma_b2 <- 4
cov_b1b2 <- 0
est<-optim(c(1,2,3,mu_b1,mu_b2,sigma_b1,sigma_b2,cov_b1b2), likelihood, method="BFGS", hessian="TRUE") # why need BFGS?
#est1<-optim(c(1,2,3,mu_b1,sigma_b1), likelihood, method="BFGS", hessian="TRUE") # why need BFGS?
# est
# fisher_info <- solve(est$hessian)
# prop_sigma <- sqrt(diag(fisher_info))
# prop_sigma
