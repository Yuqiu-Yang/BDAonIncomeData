# In this section, we will use the 
# result of the pre_logistic 
# as the prior for betas 
source("useFun.R")
library(BayesLogit)
library(coda)
library(mvtnorm)
library(progress)

load("training.RData")
d <- training

b <- reg$coefficients
B <- vcov(reg)
Binv <- solve(B)
Binv_b <- Binv%*%b

n_chains <- 10
N <- 100
post_samples_list <- vector("list", n_chains)
n_w <- nrow(d)
n_b <- length(reg$coefficients)
X <- model.matrix(reg)
k <- d$gt50 - 0.5
Xk <- crossprod(X, k)
for(nc in 1 : n_chains)
{
  post_samples <- matrix(NA, nrow = N,
                         ncol = n_b)
  
  post_samples[1, ] <- reg$coefficients
  pb <- progress_bar$new(format = paste0("Chain",nc,
                                         "[:bar] :percent eta: :eta"),
                         total = N-1, 
                         clear = FALSE, width= 60)
  for(i in 2 : N)
  {
    pb$tick()
    Xb <- tcrossprod(post_samples[i-1, ],X)
    Omega <- rpg(n_w, 1, Xb)
    V <- crossprod(X, Omega*X) + Binv
    V <- chol2inv(chol(V))
    m <- crossprod(V, Xk + Binv_b)
    post_samples[i, ] <- rmvnorm(1, mean = m, sigma = V)
    Sys.sleep(1 / 100)
  }
  post_samples_list[[nc]] <- post_samples
}

gibbs_list <- lapply(post_samples_list, mcmc)
gibbs_list <- mcmc.list(gibbs_list)
g_diag <- gelman.diag(gibbs_list)
gelman.plot(gibbs_list)

