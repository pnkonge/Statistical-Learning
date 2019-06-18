# 1) Loading the data required for Competition
getwd()
setwd("./University of Toronto - Mississauga/4 - Fourth Year/STA314/Competition/all")
sample_submission <- read.csv("SampleSubmission.csv")
test_data <- read.csv("test_predictors.csv")
training_data <- read.csv("trainingdata.csv")


# Attempt Using Decision Tress and Bagging

library(tree)
library(MASS)
library(randomForest)
library(ISLR)


# begin by slitting sample
set.seed(1)
train = sample(1:nrow(training_data), 400)                    
y.test = training_data[-train,]$y

# new arguments:
# mtry: the number of trees used at each split
# setting mtry = number of predictors corresponds to bagging
# ntree: number of trees to grow
# in lectures we called this B
# the next line does bagging with 500 replications 
bag.train = randomForest(y~X1+X2+X3+X4+X8+X12+X23+X24+X25, data = training_data, subset=train, mtry =9, ntree = 500)

# there also is a predict function
# for the output of function randomForest
# newdata should contain the new predictors  
yhat.bag = predict(bag.train,newdata=training_data[-train,])

tree.train = tree(y~X1+X2+X3+X4+X8+X12+X23+X24+X25,data=training_data,subset=train)
cv.train = cv.tree(tree.train)
best.prune = cv.train$size[which.min(cv.train$dev)]
prune.train = prune.tree(tree.train,best=best.prune)
yhat.tree = predict(prune.train, newdata=training_data[-train,])

sqrt(mean((yhat.tree - y.test)^2))
sqrt(mean((yhat.bag - y.test)^2))



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
