library(BayesLogit)
library(coda)
library(mvtnorm)
library(progress)
library(caret)
library(Rfast)
#####################################################
#####     FUNCTIONS RELATED TO MCMC CONVERGENCE
#####################################################
##############
# Gelman diagnosis and plot 
gelmanDiag <- function(post_samples_list, show_plot = T, ...)
{
  gibbs_list <- lapply(post_samples_list, mcmc)
  gibbs_list <- mcmc.list(gibbs_list)
  g_diag <- gelman.diag(gibbs_list)
  if(show_plot)
  {
    gelman.plot(gibbs_list)
  }
  return(g_diag)
}

###########
# Burn the warm up samples

burninSample <- function(post_samples_list, n_burn)
{
  return(lapply(post_samples_list,'[',-c(1:n_burn),))
}


#########
# Combine samples from different chains 
combineChains <- function(post_samples_list)
{
  rc <- dim(post_samples_list[[1]])
  temp <- matrix(NA, ncol = rc[2],
                 nrow = length(post_samples_list)*rc[1])
  for(i in 1 : (rc[2]))
  {
    temp[,i] <- as.vector(sapply(post_samples_list,'[',,i))
  }
  return(temp)
}
#####################################################
#####FUNCTIONS RELATED TO MODEL CHECKING & SELECTION
#####################################################
##############
# Convert beta to probablities

betaToP <- function(X, b)
{
  b <- matrix(b, ncol = ncol(X))
  # We will assume that each observation 
  # corresponds to each row of X
  # b can be a single vector
  # if b is matrix, then 
  # c(b0, ..., b_n) should be in the row 
  Xb <- Tcrossprod(X, b)
  p <- 1/(1 + exp(-Xb))
  return(p)
}

binaryLogisticY <- function(p)
{
  # p is n-by-m matrix 
  # m is number of mcmc 
  y <- rbinom(length(p),1, p)
  return(matrix(y, nrow = nrow(p)))
}

binaryLogisticLogLikelihood <- function(y, p)
{
  eps <- 10 * .Machine$double.eps
  p[which(p > 1 - eps)] <- 1 - eps
  p[which(p < eps)] <- eps
  return(y * log(p) + (1 - y) * log(1 - p))
}

logPointwisePredictiveDensity <- function(y, p)
{
  l <- exp(binaryLogisticLogLikelihood(y, p))
  if(!is.null(dim(l)))
  {
    l <- rowMeans(l)
  }
  temp <- log(l)
  return(sum(temp))
}

binaryLogisticLogLikelihoodPPP <- function(y_rep, y, p)
{
  l_rep <- binaryLogisticLogLikelihood(y_rep, p)
  l <- binaryLogisticLogLikelihood(y, p)
  D_rep <- colSums(l_rep)
  D <- colSums(l)
  return(mean(D_rep >= D))
}

binaryLogisticDIC <- function(y, X, b_hat, p)
{
  p_hat <- betaToP(X, b_hat)
  l_hat <- binaryLogisticLogLikelihood(y, p_hat)
  l_hat <- sum(l_hat)
  l_avg <- binaryLogisticLogLikelihood(y, p)
  l_avg <- colSums(l_avg)
  D_hat <- -2 * l_hat
  D_avg <- -2 * mean(l_avg)
  return(2 * D_avg - D_hat)
}

# This function is like a wrapper that 
# will output PPP, DIC and prdictive accuracy
PPPDICPredictiveDensity <- function(X_train, b, y_train,
                                    X_test, y_test)
{
  # We first generate y_rep
  p <- betaToP(X_train, b)
  y_rep <- binaryLogisticY(p)
  # Then we calculate the PPP 
  PPP <- binaryLogisticLogLikelihoodPPP(y_rep, y_train, p)
  rm(y_rep)
  # Then we can find the DIC 
  DIC <- binaryLogisticDIC(y_train, X_train, 
                           colMeans(b), p)
  # Then we can find the prediction accuracy 
  p <- betaToP(X_test, b)
  lppd <- logPointwisePredictiveDensity(y_test, p)
  return(list(PPP = PPP,
              DIC = DIC,
              lppd = lppd))
}




#####################################################
#####     FUNCTIONS RELATED TO MCMC SAMPLING
#####################################################

binaryLogisticPolyaGammaGibbs <- function(N, n_w, n_b, 
                            X, kappas, 
                            prior_par = list(), initials = NULL,
                            include_omega = F, nc)
{
  X_kappa <- crossprod(X, kappas)
  if(length(prior_par) <= 1)
  {
    Binv <- Binv_b <- 0
  }else{
    Binv <- solve(prior_par[[2]])
    Binv_b <- Binv %*% (prior_par[[1]])
  }
  
  post_samples_beta <- matrix(NA, nrow = N, ncol = n_b)
  if(is.null(initials))
  {
    if(length(prior_par) > 1)
    {
      post_samples_beta[1,] <- rmvnorm(1, mean = prior_par[[1]],
                                       sigma = prior_par[[2]])
    }else{
      post_samples_beta[1,] <- rmvnorm(1, mean = rep(0, n_b),
                                       sigma = 100 * diag(n_b))
    }
  }else{
    post_samples_beta[1,] <- initials
  }
  
  if(include_omega)
  {
    post_samples_omega <- matrix(NA, nrow = N, ncol = n_w)
    post_samples_omega[1,] <- rpg(n_w, 1, 0)
  }
  
  pb <- progress_bar$new(format = paste0("Chain",nc,
                                         "[:bar] :percent eta: :eta"),
                         total = N-1, 
                         clear = FALSE, width= 60)
  for(i in 2 : N)
  {
    pb$tick()
    Xb <- tcrossprod(post_samples_beta[i-1, ],X)
    Omega <- rpg(n_w, 1, Xb)
    if(include_omega)
    {
      post_samples_omega[i, ] <- Omega
    }
    V <- crossprod(X, Omega*X) + Binv
    V <- chol2inv(chol(V))
    m <- crossprod(V, X_kappa + Binv_b)
    post_samples_beta[i, ] <- mvtnorm::rmvnorm(1, mean = m, sigma = V)
    Sys.sleep(1 / 100)
  }

  if(include_omega)
  {
    return(cbind(post_samples_beta, post_samples_omega))
  }else{
    return(post_samples_beta)
  }
}



################
# Functions used to fit logistic regression
foo <- function(b, X_train, y_train)
{
  b <- matrix(b, ncol = ncol(X_train))
  p <- 1/(1 + exp(-Tcrossprod(X_train, b)))
  c_entropy <- -1 * sum(y_train * log(p) + (1 - y_train) * log(1 - p))
  return(c_entropy)
}

goo <- function(b, X_train, y_train)
{
  b <- matrix(b, ncol = ncol(X_train))
  eXb <- exp(-Tcrossprod(X_train, b))
  p <- as.numeric(1/(1 + eXb))
  dc_entropy <- colSums((p - y_train) * X_train)
  return(dc_entropy)
}


