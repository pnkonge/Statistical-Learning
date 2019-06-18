# 1) Loading the data required for Competition
getwd()
setwd("./University of Toronto - Mississauga/4 - Fourth Year/STA314/Competition/all")
sample_submission <- read.csv("SampleSubmission.csv")
test_data <- read.csv("test_predictors.csv")
training_data <- read.csv("trainingdata.csv")


# 2) Exploring the training data
summary(training_data)
head(training_data)
summary(test_data)
head(test_data)

# 3)  Attempting Ridge

#Loading packages
library(plotmo) # nicer plotting of glmnet output
library(glmnet) # this contains procedures for fitting ridge and lasso

x_train = model.matrix(y ~ . ,training_data )[,-1]  
# bring data in format that glmnet can deal with
# [,-1] is to remove intercept
# same as lecture 8

y_train = training_data$y

#grid = 10^seq(10,-2,length = 100) 

# glmnet with alpha = 1 is lasso
# note: by default, glmnet standardizes predictors
# we don't need to worry about doing that
ridge.mod = glmnet(x_train,y_train,alpha = 0, lambda = grid)
plot(ridge.mod, label = TRUE)

# nicer plot with external package
# the function glm_plot is from the library plotmo
plot_glmnet(ridge.mod)

#See Lec. 6 R- Code to show coeffiecients

# random sample from numbers 1:nrow(x)
# this will correspond to the training observations
train = sample(1:nrow(x_train),nrow(x_train)/2) 

test = (-train)
ytest = y_train[test]

# fit ridge models
fit.ri = glmnet(x_train[train,],y_train[train],alpha =0, lambda = grid)

#Cross Validation
set.seed(1)
cv.ri = cv.glmnet(x_train[train,],y_train[train],alpha =0) # cross validation for ridge

# compare predictive performance of ridge and lasso
# first extract the lambda values obtained by cross-validation
best.la = cv.ri$lambda.min

# now predict using tose lambda values
# s correspnds to lambda value
pred.ri = predict(fit.ri, s = best.la, newx = x_train[test,])

mean((pred.ri-ytest)^2)  # error of lasso



# 4) Predicting and Converting to CSV
#summary(x_test)
x_test = model.matrix(X1.500 ~ . ,test_data )[,-1]  
prediction <- predict(fit.ri, s = best.la, newx = x_test)
id = prediction[,0]
solution <- data.frame(id = c(1:500), y = prediction[,1])
names(solution)
nrow(solution)
write.csv(solution, file = "2_ridge_prediction_answer.csv", row.names = FALSE)
