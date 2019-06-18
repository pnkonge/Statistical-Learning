# 1) Loading the data required for Competition
setwd("./University of Toronto - Mississauga/4 - Fourth Year/STA314/Competition/all")
sample_submission <- read.csv("SampleSubmission.csv")
test_data <- read.csv("test_predictors.csv")
training_data <- read.csv("trainingdata.csv")
library(gbm)
set.seed(1)
train = sample(1:nrow(training_data), 400, replace = FALSE)                    
y.test = training_data[-train,]$y

#Best Error
best.err = 0
best.params = c(1,2,3)


#Vectors to iterate
#bags = seq(from=1, to=0.5, by=-0.1)
bags = c(0.6,0.7,0.8)
depth = seq(from=3,to=1,by=-1)
#depth = c(1,2)
#shrink = seq(from=0.01, to=0.05, by=0.01)
shrink = c(0.01, 0.02,0.03,0.04,0.05,0.1)


ntrees = seq(from=100 ,to=10000, by=1) #no of trees-a vector of 100 values 


#Looping through different models
count=1
num = 1
#dep=2
#bag=0.9
for (dep in depth) {
  for(bag in bags){
    for (s in shrink) {
      set.seed(1)
      boost.train = gbm(y~X1+X2+X4+X5+X6+X7+X8+X12+X23+X25, 
                        data = training_data[train,],
                        distribution='gaussian',
                        n.trees = 10000,
                        bag.fraction = bag,
                        interaction.depth = dep,
                        shrinkage = s,
                        cv.folds = 10)
      bi = gbm.perf(boost.train,method="cv", plot.it = TRUE)
      predmatrix = predict(boost.train,newdata=training_data[-train,],n.trees=bi)
      error <- sqrt(mean((predmatrix-training_data[-train,]$y)^2))
      #error
      
      if(count==1) {
        best.err = error
        best.params = c(bag, dep, s)
        count = count + 1
      }
      else if(error < best.err){
        best.err = error
        best.params = c(bag, dep, s)
        count = count + 1
        print(count)
      }
      num=num+1
      cat("Iter: ",num, ", ", "Error: ",error,", Params: [", bag,",", dep,",", s,"]")
      print(".")
    }
  }
}

cat("Iter: ",count, ", ", "Error: ",best.err,", Params: [", best.params[1],",", best.params[2],",", best.params[3],"]")


#FIRST CHECK
set.seed(1)
boost.train = gbm(y~X1+X2+X4+X5+X6+X7+X8+X12+X23+X25, 
                  data = training_data[train,],
                  distribution='gaussian',
                  n.trees = 10000,
                  bag.fraction = 0.6,
                  interaction.depth = 2,
                  shrinkage = 0.04,
                  cv.folds = 10)
bi = gbm.perf(boost.train,method="cv", plot.it = TRUE)
#bi
predmatrix = predict(boost.train,newdata=training_data[-train,],n.trees=bi)
error <- sqrt(mean((predmatrix-training_data[-train,]$y)^2))
error

predmatrix = predict(boost.train,newdata=training_data[-train,],n.trees=ntrees)
test.error <- with(training_data[-train,],apply( (predmatrix-y.test)^2,2,mean))
plot(ntrees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")
#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)

#----------------------------------------------------------------------

ntrees = seq(from=100 ,to=5000, by=1) #no of trees-a vector of 100 values 
predmatrix = predict(boost.train,newdata=training_data[-train,],n.trees=bi)
#Calculating The Mean squared Test Error
error <- sqrt(mean((predmatrix-training_data[-train,]$y)^2))
error

predmatrix = predict(boost.train,newdata=training_data[-train,],n.trees=ntrees)
dim(predmatrix)
test.error <- with(training_data[-train,],apply( (predmatrix-y.test)^2,2,mean))
head(test.error) #contains the Mean squared test error for each of the 100 trees averaged
min(test.error)
which.min(test.error)
#Plotting the test error vs number of trees


plot(ntrees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")
#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)





#------FULL MODEL PREDICTION
boost.train = gbm(y~X1+X2+X4+X5+X6+X7+X8+X12+X23+X25, 
                  data = training_data,
                  distribution='gaussian',
                  n.trees = 10000,
                  bag.fraction = 0.6,
                  interaction.depth = 2,
                  shrinkage = 0.04,
                  cv.folds = 10)
bi = gbm.perf(boost.train,method="cv", plot.it = TRUE)

#Submit Predictions
pr.boo = predict(boost.train,newdata=test_data,n.trees=bi)
solution <- data.frame(id = c(1:500), y = pr.boo)
names(solution)[2] <- "y"
names(solution)
nrow(solution)
write.csv(solution, file = "13_boosting_FINAL_ATTEMPT_Hopefully.csv", row.names = FALSE)


