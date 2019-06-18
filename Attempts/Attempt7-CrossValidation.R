# 1) Loading the data required for Competition
getwd()
setwd("./University of Toronto - Mississauga/4 - Fourth Year/STA314/Competition/all")
sample_submission <- read.csv("SampleSubmission.csv")
test_data <- read.csv("test_predictors.csv")
training_data <- read.csv("trainingdata.csv")


#Data
library(glmnet) # this contains procedures for fitting ridge and lasso
library(ISLR)   # load for data set Hitters
library(pls)    # contains functions for fitting PLS and PCR

set.seed(1)

x = model.matrix(y ~ . ,training_data)[,-1]  
y = training_data$y




#Cross Validation
R = 10

t.lm1 = numeric(R)
t.lm2 = numeric(R)
t.lm3 = numeric(R)
t.ri = numeric(R)
t.la = numeric(R)
t.nu = numeric(R)
t.ls = numeric(R)
t.pcr = numeric(R)
t.pls = numeric(R)

set.seed(1)

for(r in 1:R){
  
  train = sample(1:nrow(x),50) 
  
  # random sample from numbers 1:nrow(x)
  # this will correspond to the training observations
  test = (-train)
  ytest = y[test]
  
  fit.ri = glmnet(x[train,],y[train],alpha =0)
  fit.la = glmnet(x[train,],y[train],alpha =1)
  
  pcr.fit = pcr(y~., data=training_data ,subset = train ,scale = TRUE ,validation ="CV")
  pls.fit = plsr(y~., data=training_data ,subset = train ,scale = TRUE ,validation ="CV")
  
  sig_lm1 <- lm(y~X1+X2+X3+X4+X8+X12+X13+X23+X24+X25, data = training_data, subset = train)
  sig_with_inter_lm <- lm(y~(X1+X2+X3+X4+X8+X13+X23+X24+X25)*X12, data = training_data, subset = train)
  final_lm <- lm(y~X1+X2+(X3+X13)*X12+X4+X8+X23+X24+X25, data = training_data, subset = train)

  cv.ri = cv.glmnet(x[train,],y[train],alpha =0) # cross validation for ridge
  cv.la = cv.glmnet(x[train,],y[train],alpha =1) # cross validation for lasso
  
  
  best.ri = cv.ri$lambda.min  # best lambda value for ridge
  best.la = cv.la$lambda.min  # best lambda value for lasso
  best.pcr = which.min(MSEP(pcr.fit)$val[2,1,]) - 1 #wimi_dimred(pcr.fit,y[train]) # number of components for PCR
  best.pls = which.min(MSEP(pls.fit)$val[2,1,]) - 1 #wimi_dimred(pcr.fit,y[train]) # number of components for PLS
  
  pred.sig_lm <- predict(sig_lm1, newx = x[test,])
  pred.sig_with_inter <- predict(sig_with_inter_lm, newx = x[test,])
  pred.final_lm <- predict(final_lm, newx = x[test,])
  pred.ri = predict(fit.ri, s = best.ri, newx = x[test,])
  pred.la = predict(fit.la, s = best.la, newx = x[test,])
  lim = lm(y[train]~., data = data.frame(x[train,]))
  pred.ls = predict(lim, newdata = data.frame(x[test,]))
  pred.pcr = predict(pcr.fit ,x[test ,], ncomp = best.pcr)
  pred.pls = predict(pls.fit ,x[test ,], ncomp = best.pls)
  
  
  t.lm1[r] = sqrt(mean((pred.sig_lm -ytest)^2))
  t.lm2[r] = sqrt(mean((pred.sig_with_inter -ytest)^2))
  t.lm3[r] = sqrt(mean((pred.final_lm-ytest)^2))
  t.ri[r] = sqrt(mean((pred.ri-ytest)^2))  # error of ridge
  t.la[r] = sqrt(mean((pred.la-ytest)^2))  # error of lasso
  t.nu[r] = sqrt(mean((mean(y[train])-ytest)^2))
  t.ls[r] = sqrt(mean((pred.ls-ytest)^2))
  t.pcr[r] = sqrt(mean((pred.pcr-ytest)^2))
  t.pls[r] = sqrt(mean((pred.pls-ytest)^2))
  
  print(r)
  
}

nam = c('general lm','general lm2','general lm3','ridge', 'lasso', 'PCR', 'PLS', 'full lm')
boxplot(t.lm1, t.lm2, t.lm3, t.ri,t.la,t.pcr,t.pls,t.ls,names =  nam)


# 4) Predicting and Converting to CSV
#summary(x_test)
x_test = model.matrix(X1.500 ~ . ,test_data )[,-1]  
pred.la = predict(fit.la, s = best.la, newx = x_test)
solution <- data.frame(id = c(1:500), y = pred.la)
names(solution)[2] <- "y"
names(solution)
nrow(solution)
write.csv(solution, file = "7_cross_val_lasso_prediction.csv", row.names = FALSE)

