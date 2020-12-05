# In this section, we will use the flat
# prior for betas 
source("useFun.R")
library(BayesLogit)
library(coda)
library(mvtnorm)
library(progress)

load("training.RData")
d <- training

n_chains <- 10
N <- 2000
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
    V <- crossprod(X, Omega*X)
    V <- chol2inv(chol(V))
    m <- crossprod(V, Xk)
    post_samples[i, ] <- rmvnorm(1, mean = m, sigma = V)
    Sys.sleep(1 / 100)
  }
  post_samples_list[[nc]] <- post_samples
}

gibbs_list <- lapply(post_samples_list, mcmc)
gibbs_list <- mcmc.list(gibbs_list)
g_diag <- gelman.diag(gibbs_list)
gelman.plot(gibbs_list)

burned_post_samples_list <- lapply(post_samples_list,
                                   '[',-c(1:(N/2)),)

for(i in 1 : n_b)
{
  assign(paste("post_b",i,sep=""), 
         as.vector(sapply(burned_post_samples_list,'[',,n_w +i)))
}

temp <- kde2d(post_b1, post_b2,
              n = 200)
temp$z <- temp$z / max(temp$z)
contours = seq(.05,.95,.1)
contour(temp$x,temp$y,temp$z,levels=contours,xlab="alpha",
        ylab="beta",labcex=0.5,cex=1,
        xlim = c(-2,4.5),
        ylim = c(0, 30))
mtext("Posterior density - PG",3,line=1,cex=1.2)

