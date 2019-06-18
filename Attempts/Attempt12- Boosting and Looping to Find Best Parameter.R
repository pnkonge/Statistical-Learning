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

#Best Error
best.err = 0
best.params = c(1,2,3)


#Vectors to iterate
bags = seq(from=0.9, to=0.7, by=-0.1)
depth = seq(from=1,to=6,by=1)
shrink = seq(from=0.2, to=0.5, by=0.01)

#Looping through different models
count=1
num = 1
for (dep in depth) {
for(bag in bags){
    for (s in shrink) {
      
      boost.train = gbm(y~X1+X2+X4+X5+X7+X6+X8+X12+X23+X25, 
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
print(".")


#FIRST CHECK
boost.train = gbm(y~X1+X2+X4+X5+X7+X6+X8+X12+X23+X25, 
                  data = training_data[train,],
                  distribution='gaussian',
                  n.trees = 10000,
                  bag.fraction = 0.9,
                  interaction.depth = 1,
                  shrinkage = 0.4,
                  cv.folds = 10)
bi = gbm.perf(boost.train,method="cv", plot.it = TRUE)
bi
predmatrix = predict(boost.train,newdata=training_data[-train,],n.trees=bi)
error <- sqrt(mean((predmatrix-training_data[-train,]$y)^2))
error
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


#plot(ntrees , test.error , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")

#adding the RandomForests Minimum Error line trained on same data and similar parameters
abline(h = min(test.error),col="red") #test.err is the test error of a Random forest fitted on same data
legend("topright",c("Minimum Test error Line for Random Forests"),col="red",lty=1,lwd=1)





#------FULL MODEL PREDICTION
boost.train = gbm(y~X1+X2+X4+X5+X7+X6+X8+X12+X23+X25, 
                  data = training_data,
                  distribution='gaussian',
                  n.trees = 5000,
                  bag.fraction = 0.9,
                  interaction.depth = 1,
                  shrinkage = 0.4,
                  cv.folds = 10)
bi = gbm.perf(boost.train,method="cv", plot.it = TRUE)

#Submit Predictions
pr.boo = predict(boost.train,newdata=test_data,n.trees=bi)
solution <- data.frame(id = c(1:500), y = pr.boo)
names(solution)[2] <- "y"
names(solution)
nrow(solution)
write.csv(solution, file = "12_boosting_forloop.csv", row.names = FALSE)


