
load("training.RData")
reg <- glm(gt50 ~ .,
             data = training,
             family = binomial(logit))
