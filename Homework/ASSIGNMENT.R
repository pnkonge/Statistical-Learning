# Loading the data required for Competition
getwd()
setwd("./University of Toronto - Mississauga/4 - Fourth Year/STA314/Competition/all")
setwd("../../Homework")

sample_submission <- read.csv("SampleSubmission.csv")
test_data <- read.csv("testpredictors.csv")
training_data <- read.csv("trainingdata.csv")


# Question 1
full_lm <- lm(y~ .-X12 + as.factor(X12),data=training_data) 
summary(full_lm)
x_test = model.matrix(X1.500 ~ . ,test_data )[,-1]  
first_predictor_vector <- predict(full_lm, newx = x_test)[1]
first_predictor_vector

full_lm <- lm(y~ .,data=training_data) 
summary(full_lm)
x_test = model.matrix(X1.500 ~ . ,test_data )[,-1]  
first_predictor_vector <- predict(full_lm, newx = x_test)[1]
first_predictor_vector

#Question 2
library(leaps) # this contains the function regsubsets
library(MASS)
library(ISLR)
#-----------------------------------------------------------------------
# note: by default only search up to 8 predictors
regfit.best = regsubsets(y~.,data = training_data, nvmax = 28)
summary(regfit.best)
# to extract coefficients from one particular model, 
# for example model with 2 predictors
coef(regfit.best,2)

# note: by default only search up to 8 predictors
regfit.best = regsubsets(y~.-X12 + as.factor(X12),data = training_data, nvmax = 28)
summary(regfit.best)
# to extract coefficients from one particular model, 
# for example model with 2 predictors
coef(regfit.best,2)

# Question 3
library(gam)
gam.m1 <- gam(y~X24+ns(X1,knots = c(-1,0,1))+poly(X12, 2), data = training_data)
summary(gam.m1)
b1_estimate <- gam.m1$coefficients[2]
b1_estimate

plot(training_data$X9, training_data$y)

#Question 4
library(tree)
library(MASS)
library(randomForest)
# bagging has improved error substantially!
# more detailed comparison with other methods: later
# next consider variable importance measures
# as discussed in lectures
# run bagging on full data set
# if ntree not specified uses defualt ntree = 500
# note: need importance = TRUE
# to get %IncMSE
set.seed(1)
bag.train.full = randomForest(y~.,data = training_data, mtry = ncol(training_data)-1, importance = TRUE)
# output numerical values  
importance(bag.train.full)
# make a plot 
varImpPlot(bag.train.full) # LOOK AT THE NOTES FOR THIS -- POSSIBLY X12 (Max %IncMSE)

