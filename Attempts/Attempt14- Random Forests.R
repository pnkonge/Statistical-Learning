rm(list = ls())

library(tree)
library(MASS)
#-----------------------------------------------------------------
# recall: bagging special case of radnom forest
# thus 
# one implementation: R package randomForest
# note: other impleentations also exist
#----------------------------------------------------
library(randomForest)


# 1) Loading the data required for Competition
getwd()
setwd("./University of Toronto - Mississauga/4 - Fourth Year/STA314/Competition/all")
sample_submission <- read.csv("SampleSubmission.csv")
test_data <- read.csv("test_predictors.csv")
training_data <- read.csv("trainingdata.csv")

set.seed(1)
train = sample(1:nrow(training_data), 400, replace = FALSE)                    
y.test = training_data[-train,]$y


# the function to fit random forsts
# is called randomForest
# arguments:

# first argument: model equation, similar to linear models
# data: optional data frame that contains
# as columns variables used in formula 
# subset: use nly subset of data to train model
# all of the above similar to linear models

# new arguments:
# mtry: the number of trees used at each split
# setting mtry = number of predictors corresponds to bagging
# ntree: number of trees to grow
# in lectures we called this B

# the next line does bagging with 500 replications 
bag.train = randomForest(y~X1+X2+X4+X5+X7+X6+X8+X12+X23+X25, data = training_data, subset=train, mtry = 5, ntree = 10000)

# there also is a predict function
# for the output of function randomForest
# newdata should contain the new predictors  
yhat.bag = predict(bag.train,newdata=training_data[-train,])

# look at test MSE
sqrt(mean((yhat.bag - y.test)^2))



#-------------------------------------------------------------------------------------

# bagging has improved error substantially!
# more detailed comparison with other methods: later

# next consider variable importance measures
# as discussed in lectures
# run bagging on full data set
# if ntree not specified uses defualt ntree = 500
# note: need importance = TRUE
# to get %IncMSE
bag.train.full = randomForest(y~.,data = training_data, mtry = 13, importance = TRUE)

# output numerical values  
importance(bag.train.full)

# make a plot 
varImpPlot(bag.train.full)

#---------------------------------------------
# next take a brief look at random forests
# compared to above, we only need ot change argument mtree

rf.train_1 = randomForest(y~X1+X2+X4+X5+X7+X6+X8+X12+X23+X25, data = training_data, subset=train, mtry = 1)
rf.train_2 = randomForest(y~X1+X2+X4+X5+X7+X6+X8+X12+X23+X25, data = training_data, subset=train, mtry = 2)
rf.train_6 = randomForest(y~X1+X2+X4+X5+X7+X6+X8+X12+X23+X25, data = training_data, subset=train, mtry = 6)

yhat.rf1 = predict(rf.train_1,newdata=training_data[-train,])
yhat.rf2 = predict(rf.train_2,newdata=training_data[-train,])
yhat.rf6 = predict(rf.train_6,newdata=training_data[-train,])

mean((yhat.tree - y.test)^2)
mean((yhat.bag - y.test)^2)
mean((yhat.rf1 - y.test)^2)
mean((yhat.rf2 - y.test)^2)
mean((yhat.rf6 - y.test)^2)

# m = 6 does a bit better compared to bagging
# m = 1 not that good
# m = 2 already substantial improvement over m = 1


