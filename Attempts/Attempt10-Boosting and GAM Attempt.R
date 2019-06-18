# 1) Loading the data required for Competition
getwd()
setwd("./University of Toronto - Mississauga/4 - Fourth Year/STA314/Competition/all")
sample_submission <- read.csv("SampleSubmission.csv")
test_data <- read.csv("test_predictors.csv")
training_data <- read.csv("trainingdata.csv")

library(gbm)
set.seed(1)
train = sample(1:nrow(training_data), 400, replace = FALSE)                    
y.test = training_data[-train,]$y


#Validation set approach -- Changes to Training Model
boost.train = gbm(y~X1+X2+X4+X5+X7+X6+X8+X12+X23+X25, 
                  data = training_data[train,],
                  distribution='gaussian',
                  n.trees = 5000,
                  bag.fraction = 1,
                  interaction.depth = 1,
                  shrinkage = 0.5,
                  cv.folds = 10)
bi = gbm.perf(boost.train,method="cv", plot.it = TRUE)
ntrees = seq(from=100 ,to=5000, by=1) #no of trees-a vector of 100 values 
predmatrix = predict(boost.train,newdata=training_data[-train,],n.trees=bi)
dim(predmatrix)
#Calculating The Mean squared Test Error
error <- sqrt(mean((predmatrix-training_data[-train,]$y)^2))
test.error<-with(training_data[-train,],sqrt(apply( (predmatrix-y)^2,2,mean)))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged
min(test.error)
which.min(test.error)
#Plotting the test error vs number of trees
plot(ntrees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)
#---------------------------------------------------




#------FULL MODEL PREDICTION
boost.train = gbm(y~X1+X2+X4+X5+X7+X6+X8+X12+X23+X25, 
                  data = training_data,
                  distribution='gaussian',
                  n.trees = 5000,
                  bag.fraction = 1,
                  interaction.depth = 1,
                  shrinkage = 0.5,
                  cv.folds = 10)
bi = gbm.perf(boost.train,method="cv")
#?gbm.perf


#Submit Predictions
pr.boo = predict(boost.train,newdata=test_data,n.trees=bi)
solution <- data.frame(id = c(1:500), y = pr.boo)
names(solution)[2] <- "y"
names(solution)
nrow(solution)
write.csv(solution, file = "10_boosting.csv", row.names = FALSE)




#---------------
# random forest with m = 6
# on same split gives similar performance

# plot influence of rm on prediction
# this is avergaed over all other predictors
# see details on lecture slides
plot(boost.train,i='X12',n.trees = bi)
plot(boost.train,i='X25',n.trees = bi)

# heat plot of joint effect of rm and lstat
plot(boost.train,i=c('X12','X25'),n.trees = 500)
