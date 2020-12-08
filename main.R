#######################################
# In the main section, we will 
# First: Fit a logistic reg: pre_reg on the pre_training dataset 
#        the purpose is to generate a prior 
# Second: 
#        1: Fit a logistic reg: reg on the training dataset
#        2: Fit a logistic reg using the PG with a flat prior
#        3: Use the result from pre_reg to form a normal prior 
#           and use the Polya-Gamma to fit a logistic reg
#        4: We will detect if MCMC chains have converged 
#        5: We will compare model performance
#######################################
# We will remove everything first just in case 
rm(list = ls())
source("useFun.R")

######
# First step 
load("pre_training.RData")
pre_reg <- glm(gt50 ~ .,
               data = pre_training,
               family = binomial(logit))
rm(pre_training)

######
# Second step 
######
# 1. We fit a logistic regression 
load("training.RData")
d <- training
rm(training)
reg <- glm(gt50 ~ .,
           data = d,
           family = binomial(logit))
load("test.RData")
lppd_reg <- logPointwisePredictiveDensity(test[,8], predict(reg, 
                                                newdata = test[,-8],
                                                type = "response"))

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
# We check if chains have converged 
g_diag_flat <- gelmanDiag(post_samples_list_flat)
# According to the Gelman analysis, 
# it seems that all the parameters 
# have converged 
# We then burn the first half of each chain 
# and combine all the chains 
burned_post_samples_list_flat <- burninSample(post_samples_list_flat,
                                              N/2)
burned_post_samples_list_flat <- combineChains(burned_post_samples_list_flat)

# We will then use posterior predictive p value 
# based on deviance (log likelihood) to 
# check the model fitting 
# Find DIC and predictive accuracy
X_test <- model.matrix(gt50~. ,data = test)
flat_list <- PPPDICPredictiveDensity(X, burned_post_samples_list_flat,
                                d[,8], X_test, test[,8])


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

############
g_diag_normal <- gelmanDiag(post_samples_list_normal)
burned_post_samples_list_normal <- burninSample(post_samples_list_normal,
                                              N/2)
burned_post_samples_list_normal <- combineChains(burned_post_samples_list_normal)


normal_list <- PPPDICPredictiveDensity(X, burned_post_samples_list_normal,
                                     d[,8], X_test, test[,8])





