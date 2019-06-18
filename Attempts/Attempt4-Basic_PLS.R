#install.packages('glmnet')
#install.packages('ISLR')
#install.packages('pls')

library(glmnet) # this contains procedures for fitting ridge and lasso
library(ISLR)   # load for data set Hitters
library(pls)    # contains functions for fitting PLS and PCR

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

# 3) Attempting PCR and PLS


x = model.matrix(X1.500~.,data = test_data)[,-1]  
y = training_data$y

lm.fit = lm(y~.,data = training_data)
pcr.fit = pcr(y~., data=training_data, scale = TRUE ,validation ="CV")
pls.fit = plsr(y~., data=training_data, scale = TRUE ,validation ="CV")


best.pcr = which.min(MSEP(pcr.fit)$val[2,1,]) - 1 #wimi_dimred(pcr.fit,y[train]) # number of components for PCR
best.pls = which.min(MSEP(pls.fit)$val[2,1,]) - 1 #wimi_dimred(pcr.fit,y[train]) # number of components for PLS

pred.ls = predict(lm.fit, newdata = data.frame(x))
pred.pcr = predict(pcr.fit ,x, ncomp = best.pcr)
pred.pls = predict(pls.fit ,x, ncomp = best.pls)

summary(lm.fit)


# 4) Predicting and Converting to CSV
#summary(x_test)
#x_test = model.matrix(X1.500 ~ . ,test_data )[,-1]  
#prediction <- predict(fit.la, s = best.la, newx = x_test)
#id = prediction[,0]
solution <- data.frame(id = c(1:500), y = pred.pls )
names(solution)[2]<-"y"
nrow(solution)
write.csv(solution, file = "4_basic_pls_answer.csv", row.names = FALSE)
