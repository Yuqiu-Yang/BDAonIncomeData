###########
# In this section, we will only use 
# the pre-training dataset
# The purpose of this is to 
# generate a prior

load("pre_training.RData")
pre_reg <- glm(income ~ .,
           data = pre_training,
           family = binomial(logit))
X <- model.matrix(pre_reg)



