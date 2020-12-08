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

a <- numeric(1000)
for(i in 1 : 1000)
{
  d <- stratified(training, group = c(2:7), size = 1)
  d_subset <- stratified(d, group = c(2,3), size = 2)
  
  reg <- glm(gt50 ~.,
             data = d,
             family = binomial(logit))
  reg_subset <- glm(gt50 ~ .,
             data = d_subset,
             family = binomial(logit))
  temp <- predict(reg, test[,c(1,2)], type = "response")
  temp <- as.numeric(temp >= 0.5)
  a[i] <- sum(temp == test[,8])/nrow(test)
}


####################
# In both cases, we will run 10 MCMC chains
n_chains <- 10
N <- 2000
# The number of omega's is equal to the # of obs
n_w <- nrow(d)
# The number of beta's is the same in both cases 
n_b <- length(reg$coefficients)
# We extract the design matrix 
X <- model.matrix(reg)
# The kappa values only dependent on the data
kappas <- d$gt50 - 0.5
####################
# 2. We use a flat prior
post_samples_list_flat <- vector("list", n_chains)
for(nc in 1 : n_chains)
{
  post_samples_list_flat[[nc]] <- 
    binaryLogisticPolyaGammaGibbs(N, n_w, n_b,
                                  X, kappas, nc = nc)
}

# 3. We use a normal prior
post_samples_list_normal <- vector("list", n_chains)
for(nc in 1 : n_chains)
{
  post_samples_list_normal[[nc]] <- 
    binaryLogisticPolyaGammaGibbs(N, n_w, n_b,
                                  X, kappas, 
                                  prior_par = list(pre_reg$coefficients,
                                                   vcov(pre_reg)),
                                  initials = pre_reg$coefficients,
                                  nc = nc)
}





