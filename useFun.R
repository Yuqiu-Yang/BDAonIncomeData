library(BayesLogit)
library(coda)
library(mvtnorm)
library(progress)

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

binaryLogisticPolyaGammaGibbs <- function(N, n_w, n_b, 
                            X, kappas, 
                            prior_par = list(), initials = NULL,
                            include_omega = F)
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
    m <- crossprod(V, Xk + Binv_b)
    post_samples_beta[i, ] <- rmvnorm(1, mean = m, sigma = V)
    Sys.sleep(1 / 100)
  }

  if(include_omega)
  {
    return(cbind(post_samples_beta, post_samples_omega))
  }else{
    return(post_samples_beta)
  }
}


