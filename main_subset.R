#######################################
# In the main_subset section, we will 
# Choose a small subset of the training data
# and compare model performance 


rm(list = ls())
source("useFun.R")

load("pre_training.RData")
pre_reg <- glm(gt50 ~ .,
               data = pre_training,
               family = binomial(logit))
rm(pre_training)


load("training.RData")
d <- training
rm(training)
X <- model.matrix(gt50~., data = d)
load("test.RData")
X_test <- model.matrix(gt50~. ,data =test)

for(trial in 1 : 100)
{
  result <- list()
  
  ind <- sample(1 : nrow(X), 400)
  result$ind <- ind
  
  X_train <- X[ind, ]
  y_train <- d[ind, 8]
  missing_level <- any(colSums(X_train) == 0)
  
  result$has_missing <- missing_level
  options(warn = 2)
  lg <- try(glm(gt50 ~., data = d[ind,], family = binomial(logit)),
            silent = T)
  options(warn = 0)
  result$prob0_1 <- (class(lg) == "try-error")
  
  is_error <- T
  while(is_error)
  {
    reg <- try(optim(rnorm(18), foo,goo,
                     X_train = X_train, 
                     y_train = y_train,
                     control = list(maxit = 5000),
                     method = "BFGS"), silent = T)
    is_error <- (class(reg) == "try-error")
  }
  p_reg <- 1/(1 + exp(-X_test %*% reg$par))
  lppd_reg <- logPointwisePredictiveDensity(test[,8], p_reg)
  result$lppd_reg <- lppd_reg
  result$reg <- reg
  
  ####################
  # In both cases, we will run 10 MCMC chains
  n_chains <- 10
  N <- 2000
  # The number of omega's is equal to the # of obs
  n_w <- nrow(X_train)
  # The number of beta's is the same in both cases 
  n_b <- length(reg$par)
  # The kappa values only dependent on the data
  kappas <- y_train - 0.5
  ####################
  # 2. We use a flat prior
  post_samples_list_flat <- vector("list", n_chains)
  prior_par <- list()
  if(missing_level)
  {
    prior_par <- list(rep(0, n_b), diag(100, n_b))
  }
  for(nc in 1 : n_chains)
  {
    post_samples_list_flat[[nc]] <- 
      binaryLogisticPolyaGammaGibbs(N, n_w, n_b,
                                    X_train, kappas, 
                                    prior_par = prior_par,
                                    nc = nc)
  }
  g_diag_flat <- gelmanDiag(post_samples_list_flat,
                            show_plot = F)
  burned_post_samples_list_flat <- burninSample(post_samples_list_flat,
                                                N/2)
  burned_post_samples_list_flat <- combineChains(burned_post_samples_list_flat)
  flat_list <- PPPDICPredictiveDensity(X_train, burned_post_samples_list_flat,
                                       y_train, X_test, test[,8])
  result$g_diag_flat <- g_diag_flat
  result$check_flat <- flat_list
  result$flat_sample <- post_samples_list_flat
  
  # 3. We use a normal prior
  post_samples_list_normal <- vector("list", n_chains)
  for(nc in 1 : n_chains)
  {
    post_samples_list_normal[[nc]] <- 
      binaryLogisticPolyaGammaGibbs(N, n_w, n_b,
                                    X_train, kappas, 
                                    prior_par = list(pre_reg$coefficients,
                                                     vcov(pre_reg)),
                                    initials = pre_reg$coefficients,
                                    nc = nc)
  }
  g_diag_normal <- gelmanDiag(post_samples_list_normal,
                              show_plot = F)
  burned_post_samples_list_normal <- burninSample(post_samples_list_normal,
                                                  N/2)
  burned_post_samples_list_normal <- combineChains(burned_post_samples_list_normal)
  
  
  normal_list <- PPPDICPredictiveDensity(X_train, burned_post_samples_list_normal,
                                         y_train, X_test, test[,8])

  result$g_diag_normal <- g_diag_normal
  result$check_normal <- normal_list
  result$normal_sample <- post_samples_list_normal
  save(result, file = paste0("./trials/trial_",trial))
  print(trial)
}




result_summary <- data.frame(matrix(NA, 
                                    ncol = 11,
                                    nrow = 100))
colnames(result_summary) <- c("has_missing", "prob0_1",
                              "lppd_reg", "flat_gelman",
                              "flat_PPP", "flat_DIC", "flat_lppd",
                              "normal_gelman",
                              "normal_PPP", "normal_DIC", "normal_lppd")
for(trial in 1 : 100)
{
  load(paste0("./trials/trial_",trial))
  result_summary[trial,] <- c(result$has_missing,
                                result$prob0_1[1],
                                result$lppd_reg,
                                result$g_diag_flat$mpsrf,
                                result$check_flat$PPP,
                                result$check_flat$DIC,
                                result$check_flat$lppd,
                                result$g_diag_normal$mpsrf,
                                result$check_normal$PPP,
                                result$check_normal$DIC,
                                result$check_normal$lppd)
}

############
# We first check the model performance when there is 
# no missing level and no extreme probability
ind <- which((result_summary$has_missing == 0) & 
               (result_summary$prob0_1 == 0))
temp <- data.frame(matrix(NA, ncol = 2,
                          nrow = length(ind) *2))
temp[,1] <- c(result_summary$flat_PPP[ind],
              result_summary$normal_PPP[ind])
temp[,2] <- rep(c("flat","normal"), each = length(ind))
boxplot(temp[,1] ~ temp[,2])


