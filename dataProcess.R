library(splitstackshape)
library(car)
library(countrycode)
c_names <- c("age", "workclass", "fnlwgt",
             "education", "education_num",
             "marital_status", "occupation",
             "relationship", "race",
             "sex", "capital_gain",
             "capital_loss", "hours_per_week",
             "native_country", "income")
training_data <- read.table("adult.data", sep = ",",
                       col.names = c_names,
                       na.strings = "?",
                       stringsAsFactors = T,
                       strip.white = T)
test <- read.table("adult.test", skip = 1,
                   sep = ",",
                   col.names = c_names,
                   na.strings = "?", 
                   stringsAsFactors = T,
                   strip.white = T)
########################################
# We will only keep records without NAs
########################################
training_data <- training_data[complete.cases(training_data),]
test <- test[complete.cases(test),]
training_data$gt50 <- as.numeric(grepl(">", training_data$income, fixed = T))
test$gt50 <- as.numeric(grepl(">", test$income, fixed = T))

str(training_data)
########################################
# We will then discard variables that 
# does not make a whole lot sense,
# is kinda redundant given other vars,
# or contains lots of 0's 
# We will throw away fnlwgt, relationship, 
# workclass, capital gain, and capital loss
#######################################
training_data <- subset(training_data, 
                        select = -c(fnlwgt, workclass, relationship, race,
                                    capital_gain,capital_loss, income))
test <- subset(test, 
                  select = -c(fnlwgt, workclass, relationship, race,
                              capital_gain, capital_loss, income))
########################################
# We see that education and education_num
# encode the exact same info
# plot(training_data$education, training_data$education_num)
# temp <- list()
# for(i in levels(training_data$education))
# {
#   temp[[i]] <- unique(training_data$education_num[training_data$education==i])
# }
# sapply(temp, length)
#
# This means that we only need one of them 
# we will keep the education
########################################
training_data <- subset(training_data, select = -c(education_num))
test <- subset(test, select = -c(education_num))
########################################
########################################
########################################
################################
# We can combine factor levels 
################################

################################
# Out of 30000 records, around 
# 27000 has the native country: the US
# we will recode all non-US to non-US
################################
temp <- levels(training_data$native_country)
temp <- temp[which(temp!="United-States")]
temp <- paste(temp, collapse = ",")
temp <- gsub(",", "','",temp)
temp <- paste0("c('", temp, "')='non-US'")
training_data$native_country <- recode(training_data$native_country,
                                       temp)
test$native_country <- recode(test$native_country,
                              temp)
################################
## We will then combine anything up to 12th to pre-high
################################
training_data$education <- recode(training_data$education,
                                  'c("Preschool", "1st-4th", "5th-6th",
          "7th-8th", "9th", "10th", "11th",
          "12th") = "Prehigh"')
training_data$education <- recode(training_data$education,
                                  'c("Assoc-acdm","Assoc-voc") = "Associate"')
training_data$education <- recode(training_data$education,
                                  'c("Masters","Doctorate") = "Graduate"')

test$education <- recode(test$education,
                                  'c("Preschool", "1st-4th", "5th-6th",
          "7th-8th", "9th", "10th", "11th",
          "12th") = "Prehigh"')
test$education <- recode(test$education,
                          'c("Assoc-acdm","Assoc-voc") = "Associate"')
test$education <- recode(test$education,
                          'c("Masters","Doctorate") = "Graduate"')


################################
# The majority of the data has 
# hours per week = 40
# We also group them
################################
training_data$hours_per_week <- cut(training_data$hours_per_week,
                         breaks = c(0, 39, 40, 100),
                         labels = c("<40", "40", ">40"))

test$hours_per_week <- cut(test$hours_per_week,
                                    breaks = c(0, 39, 40, 100),
                           labels = c("<40", "40", ">40"))

################################
# We will group occupation
###############################
training_data$occupation <- recode(training_data$occupation,
                                  'c("Farming-fishing",
                                  "Machine-op-inspct",
                                  "Armed-Forces",
                                  "Transport-moving") = "Production"')
training_data$occupation <- recode(training_data$occupation,
                                   'c("Adm-clerical",
                                  "Craft-repair",
                                  "Exec-managerial",
                                  "Handlers-cleaners",
                                  "Other-service",
                                  "Priv-house-serv",
                                  "Prof-specialty",
                                  "Protective-serv",
                                   "Sales", "Tech-support") = "Service"')
test$occupation <- recode(test$occupation,
                                   'c("Farming-fishing",
                                  "Machine-op-inspct",
                                  "Armed-Forces",
                                  "Transport-moving") = "Production"')
test$occupation <- recode(test$occupation,
                                   'c("Adm-clerical",
                                  "Craft-repair",
                                  "Exec-managerial",
                                  "Handlers-cleaners",
                                  "Other-service",
                                  "Priv-house-serv",
                                  "Prof-specialty",
                                  "Protective-serv",
                                   "Sales", "Tech-support") = "Service"')
################################
# we will group marital status
################################
training_data$marital_status <- recode(training_data$marital_status,
                                       'c("Married-AF-spouse",
                                       "Married-civ-spouse") = "Married"')
test$marital_status <- recode(test$marital_status,
                                       'c("Married-AF-spouse",
                                       "Married-civ-spouse") = "Married"')


                                   
                                   
                                   
for(i in 1 : ncol(training_data))
{
  barplot(table(training_data[,i]),
          main = colnames(test)[i])
}


# can use stratified sampling 
# here we will use a simple random sample 
set.seed(42)
n <- 3000
ind <- sample(1:nrow(training_data), n)
pre_training <- training_data[ind, ]
training <- training_data[setdiff(1:nrow(training_data),
                                  ind),]

##
save(pre_training, file = "pre_training.RData")
save(training, file = "training.RData")
save(test, file = "test.RData")





##################
# OUTDATED METHOD 
##################
################################
# We also group age
################################
training_data$age <- cut(training_data$age,
                         breaks = c(10, 20, 30, 40, 50, 60, 100))
test$age <- cut(test$age,
                breaks = c(10, 20, 30, 40, 50, 60, 100))

###########################
# We can also group countries into regions
# training_data$native_country <- as.character(training_data$native_country)
# training_data <- training_data[which(training_data$native_country!="South"),]
# training_data$native_country <- countrycode(sourcevar = training_data$native_country, 
#                                             origin = "country.name",
#                                             destination = "region",
#                                             custom_match = c("England" = "Europe & Central Asia",
#                                                              "Hong" = "East Asia & Pacific",
#                                                              "Scotland" = "Europe & Central Asia",
#                                                              "Columbia" = "Latin America & Caribbean"))
training_data$native_country <- as.factor(training_data$native_country)

# test$native_country <- as.character(test$native_country)
# test <- test[which(test$native_country!="South"),]
# test$native_country <- countrycode(sourcevar = test$native_country, 
#                                    origin = "country.name",
#                                    destination = "region",
#                                    custom_match = c("England" = "Europe & Central Asia",
#                                                     "Hong" = "East Asia & Pacific",
#                                                     "Scotland" = "Europe & Central Asia",
#                                                     "Columbia" = "Latin America & Caribbean"))
test$native_country <- as.factor(test$native_country)

### This file is to build a function that can 
### 1. create dummy variables
### 2. remove variables with high correlation and near zero variance

### Input: data matrix for the regression
### Output: data matrix after removing variables high correlation and near zero variance

# Import library
library(caret)

varProcess <- function(df, formula, response){
  response_col = df[,response]
  
  # create dummy variables for factor variables
  
  df_temp = model.matrix(formula, data = df)
  
  
  # remove variables with near zero variance 
  NearZeroCols = colnames(df_temp)[nearZeroVar(df_temp)]
  print("Remove below variables for near zero variance")
  print(NearZeroCols)
  df_temp = df_temp[ ,-nearZeroVar(df_temp)]
  
  
  # remove variables with high correlation
  segCorr <- cor(df_temp)
  highCorr <- findCorrelation(segCorr, 0.9)
  HighCorrCols = colnames(df_temp)[highCorr]
  print("Remove below variables for high correlation")
  print(HighCorrCols)
  if (length(HighCorrCols) > 0) {
    df_temp = df_temp[ ,-highCorr]
  }
  
  
  df_temp = cbind(df_temp,response_col)
  colnames(df_temp)[colnames(df_temp) == "response_col"] = "gt50"
  df_temp = data.frame(df_temp)
  return(df_temp)
}

df = rbind(training_data, test)
df_var <- varProcess(df, gt50 ~ ., "gt50")
training_data = df_var[1:27162,]
test = df_var[27163:42222,]
