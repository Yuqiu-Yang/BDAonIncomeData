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
# We will only keep records without NAs
training_data <- training_data[complete.cases(training_data),]
test <- test[complete.cases(test),]
str(training_data)
################################
# We can combine factor levles 
################################
# First we see that education and education_num
# encode the exact same info 
plot(training_data$education, training_data$education_num)
temp <- list()
for(i in levels(training_data$education))
{
  temp[[i]] <- unique(training_data$education_num[training_data$education==i])
}
sapply(temp, length)
# This means that we only need one of them 
# we will keep the education
training_data <- subset(training_data, select = -c(education_num))
test <- subset(test, select = -c(education_num))
## We will then combine anything up to 12th to pre-high
training_data$education <- recode(training_data$education,
                                  'c("Preschool", "1st-4th", "5th-6th",
          "7th-8th", "9th", "10th", "11th",
          "12th") = "Prehigh"')
test$education <- recode(test$education,
                                  'c("Preschool", "1st-4th", "5th-6th",
          "7th-8th", "9th", "10th", "11th",
          "12th") = "Prehigh"')
# We also group age
# Adolescence (13-18 years), 
# Adult (19-59 years) 
# Senior Adult (60 years and above)
training_data$age <- cut(training_data$age,
                         breaks = seq(0, 100, by = 10))
test$age <- cut(test$age,
                         breaks = seq(0, 100, by = 10))
###########################
# We can also group countries into regions
training_data$native_country <- as.character(training_data$native_country)
training_data <- training_data[which(training_data$native_country!="South"),]
training_data$native_country <- countrycode(sourcevar = training_data$native_country, 
            origin = "country.name",
            destination = "region",
            custom_match = c("England" = "Europe & Central Asia",
                             "Hong" = "East Asia & Pacific",
                             "Scotland" = "Europe & Central Asia",
                             "Columbia" = "Latin America & Caribbean"))
training_data$native_country <- as.factor(training_data$native_country)

test$native_country <- as.character(test$native_country)
test <- test[which(test$native_country!="South"),]
test$native_country <- countrycode(sourcevar = test$native_country, 
                                            origin = "country.name",
                                            destination = "region",
                                            custom_match = c("England" = "Europe & Central Asia",
                                                             "Hong" = "East Asia & Pacific",
                                                             "Scotland" = "Europe & Central Asia",
                                                             "Columbia" = "Latin America & Caribbean"))
test$native_country <- as.factor(test$native_country)
#######
#




# can use stratified sampling 
# here we will use a simple random sample 
set.seed(42)
n <- 2000
ind <- sample(1:nrow(training_data), n)
pre_training <- training_data[ind, ]
training <- training_data[setdiff(1:nrow(training_data),
                                  ind),]




