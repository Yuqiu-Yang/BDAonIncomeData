# In this section, we will use the 
# result of the pre_logistic 
# as the prior for betas 


library(BayesLogit)
library(coda)
library(mvtnorm)

load("training.RData")
# generate initial values 

n_chains <- 10
N <- 10000
post_samples_list <- vector("list", n_chains)
# number of w's is equal to # of obs
n_w <- nrow(training)
n_b <- length(reg$coefficients)
X <- model.matrix(reg)


b <- reg$coefficients
B <- vcov(reg)


for(chain in n_chains)
{
  post_samples <- data.frame(matrix(NA, nrow = N,
                                    ncol = n_w + n_b))
  # initialize w_i's
  post_samples[1, 1 : n_w] <- rpg(n_w, 1, 0)
  # initialize beta's 
  post_samples[1, (n_w + 1) : (n_w + n_b)] <- 
    rmvnorm(1, mean = b, sigma = B)
  for(i in 2 : N)
  {
    
  }
  
  post_samples_list[[chain]] <- post_samples
}




