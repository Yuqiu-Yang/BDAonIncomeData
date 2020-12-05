
load("training.RData")
reg <- glm(income ~ .,
             data = training,
             family = binomial(logit))
