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
#-------------------------------------IGNORE----------------------------------------------

# 3) Some basic modelling -- NOT A GOOD IDEA
basic_lm <- lm(y~ .,data=training_data) #wiithout interactions
basic_lm$coefficients
summary(basic_lm)

basic_lm2 <- lm(y~.*.,data=training_data)
basic_lm2$coefficients
summary(basic_lm2)


# 3) b. SPLIT TRAINING DATA INTO TEST AND TRAINING

n <- length(training_data) #Sample Size
# permute index set
permidx = sample(1:n,n)
new_train = training_data[-permidx,]
validation_set = training_data[permidx,]

#Modelling New Training Data and Predicting Validation Set
basic_lm <- lm(y~ .,data= new_train) #wiithout interactions
basic_lm$coefficients
#summary(basic_lm)

predicted_y <- predict(basic_lm, newdata = validation_set)

mean_err <- mean((validation_set$y-predicted_y)^2)
mean_err
#-------------------------------------STOP IGNORING----------------------------------------------


# 3) c. Attempting LASSO

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
lasso.mod = glmnet(x_train,y_train,alpha = 1, lambda = grid)
plot(lasso.mod, label = TRUE)

# nicer plot with external package
# the function glm_plot is from the library plotmo
plot_glmnet(lasso.mod)

#See Lec. 6 R- Code to show coeffiecients

# random sample from numbers 1:nrow(x)
# this will correspond to the training observations
train = sample(1:nrow(x_train),nrow(x_train)/2) 

test = (-train)
ytest = y_train[test]

# fit ridge and lasso models
fit.la = glmnet(x_train[train,],y_train[train],alpha =1, lambda = grid)

#Cross Validation
set.seed(1)
cv.la = cv.glmnet(x_train[train,],y_train[train],alpha =1) # cross validation for lasso

# compare predictive performance of ridge and lasso
# first extract the lambda values obtained by cross-validation
best.la = cv.la$lambda.min

# now predict using tose lambda values
# s correspnds to lambda value
pred.la = predict(fit.la, s = best.la, newx = x_train[test,])

mean((pred.la-ytest)^2)  # error of lasso



# 4) Predicting and Converting to CSV
#summary(x_test)
x_test = model.matrix(X1.500 ~ . ,test_data )[,-1]  
prediction <- predict(fit.la, s = best.la, newx = x_test)
id = prediction[,0]
lasso_solution <- data.frame(id = c(1:500), y = prediction[,1])
names(lasso_solution)
nrow(lasso_solution)
write.csv(lasso_solution, file = "1_lasso_prediction_answer.csv", row.names = FALSE)
