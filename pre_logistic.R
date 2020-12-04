library(logistf)

reg <- logistf(income ~ age + workclass +
             fnlwgt + education + 
             marital_status + occupation + 
             relationship + race + 
             sex + capital_gain+
             capital_loss + 
             hours_per_week + 
             native_country,
           data = pre_training,
           control = logistf.control(maxit = 10000))

