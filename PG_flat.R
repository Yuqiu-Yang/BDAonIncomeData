library(BayesLogit)
library(coda)
library(mvtnorm)
library(logistf)



d <- data.frame(doses=c(-.863,-.296,-.053,.727),
                rats=c(5,5,5,5),deaths=c(0,1,3,5))

reg <- glm(cbind(deaths, rats-deaths) ~ doses, 
           family=binomial(logit),data=d)
n_chains <- 10
N <- 2000
post_samples_list <- vector("list", n_chains)
n_w <- nrow(d)
n_b <- length(reg$coefficients)
X <- model.matrix(reg)
k <- d$deaths - d$rats/2
for(nc in 1 : n_chains)
{
  post_samples <- data.frame(matrix(NA, nrow = N,
                                    ncol = n_w + n_b))
  colnames(post_samples) <- c(paste("w",1:n_w, sep="_"),
                              paste("beta", 1: n_b, sep="_"))
  for(i in 1 : n_w)
  {
    post_samples[1, i] <- rpg(1, d$rats[i])
  }
  post_samples[1, (n_w + 1) : (n_w + n_b)] <- reg$coefficients
  for(i in 2 : N)
  {
    Xb <- X %*% unlist(post_samples[i-1, (n_w + 1) : (n_w + n_b)])
    for(j in 1 : n_w)
    {
      post_samples[i, j] <- rpg(1, d$rats[j], Xb[j,1])
    }
    Omega <- diag(post_samples[i, 1:n_w])
    V <- solve(t(X) %*% Omega %*% X)
    m <- V %*% (t(X) %*% k )
    post_samples[i, (n_w + 1) : (n_w + n_b)] <- rmvnorm(1, mean = m, sigma = V)
  }
  print(nc)
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

