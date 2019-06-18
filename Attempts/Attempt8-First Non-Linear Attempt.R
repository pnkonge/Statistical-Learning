# 1) Loading the data required for Competition
getwd()
setwd("./University of Toronto - Mississauga/4 - Fourth Year/STA314/Competition/all")
sample_submission <- read.csv("SampleSubmission.csv")
test_data <- read.csv("test_predictors.csv")
training_data <- read.csv("trainingdata.csv")


#Data
library(ISLR)
library (gam)

#Shuffling Data Set
set.seed(2)
test_shuffle = sample(c(1:500), replace = FALSE, size = 500)


R=5
gam1_err = numeric(R)
gam2_err = numeric(R)
gam3_err = numeric(R)
gam4_err = numeric(R)
gam5_err = numeric(R)
gam6_err = numeric(R)


for(r in 1:R){
  test = test_shuffle[((r-1)*100+1):(r*100)]
  
  gam <- gam(y~., data = training_data[-test,])
  gam2 <- gam(y~X1+X2+X3+X4+X8+X12+X23+X24+X25, data = training_data[-test,])
  gam3 <- gam(y~X1+X2+X3+X4+X8+ns(X12,3)+X23+X24+X25, data = training_data[-test,])
  gam4 <- gam(y~.-X12+poly(X12,3), data = training_data[-test,])
  gam5 <- gam(y~X1+X2+X3+X4+X8+poly(X12,3)+X23+X24+X25, data = training_data[-test,])
  gam6 <- gam(y~X1+X2+X3+X4+X8+bs(X12,3)+X23+X24+X25, data = training_data[-test,])
  #summary(gam3)
  anova(gam, gam2, gam3,gam4, gam5, gam6)
  
  pred_gam1 <- predict (gam,newdata =training_data[test,])
  pred_gam2 <- predict (gam2,newdata =training_data[test,])
  pred_gam3 <- predict (gam3,newdata =training_data[test,])
  pred_gam4 <- predict (gam4,newdata =training_data[test,])
  pred_gam5 <- predict (gam5,newdata =training_data[test,])
  pred_gam6 <- predict (gam6,newdata =training_data[test,])
  
  gam1_err[r] <- sqrt(mean((training_data[test,]$y-pred_gam1)^2))
  gam2_err[r] <- sqrt(mean((training_data[test,]$y-pred_gam2)^2))
  gam3_err[r] <- sqrt(mean((training_data[test,]$y-pred_gam3)^2))
  gam4_err[r] <- sqrt(mean((training_data[test,]$y-pred_gam4)^2))
  gam5_err[r] <- sqrt(mean((training_data[test,]$y-pred_gam5)^2))
  gam6_err[r] <- sqrt(mean((training_data[test,]$y-pred_gam6)^2))
  

}


nam = c('Gam 1', 'Gam 2', 'Gam 3', 'Gam 4', 'Gam 5', 'Gam 6')
boxplot(gam1_err, gam2_err, gam3_err, gam4_err, gam5_err, gam6_err,names =  nam)


gam3 <- gam(y~X1+X2+X3+X4+X8+ns(X12,3)+X23+X24+X25, data = training_data)


#Predictions
x_test = model.matrix(X1.500 ~ . ,test_data )[,-1]  
prediction <- predict (gam3, newdata = test_data)
solution <- data.frame(id = c(1:500), y = prediction)
names(solution)[2] <- "y"
names(solution)
nrow(solution)
write.csv(solution, file = "8_gam_cross_validation.csv", row.names = FALSE)


