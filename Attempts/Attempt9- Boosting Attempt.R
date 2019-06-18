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

boost.train = gbm(y~., 
                    data = training_data[train,],
                    distribution='gaussian',
                    n.trees = 5000,
                    bag.fraction = 1,
                    interaction.depth = 1,
                    shrinkage = 0.1,
                    cv.folds = 5)


# bi gives relative variable importance
#summary(boost.train)
# next line gives results from cross-validation
bi = gbm.perf(boost.train,method="cv")
bi
# predictions 
# note: once we have a model with N trees
# can specify any number of trees for prediction
# as long as it does not exceed N
pr.boo = predict(boost.train,newdata=training_data[-train,],n.trees=bi)
error = sqrt(mean((pr.boo-y.test)^2))
error


#------FULL MODEL PREDICTION
boost.train = gbm(y~., 
                  data = training_data,
                  distribution='gaussian',
                  n.trees = 5000,
                  bag.fraction = 1,
                  interaction.depth = 1,
                  shrinkage = 0.1,
                  cv.folds = 5)
bi = gbm.perf(boost.train,method="cv")



#Submit Predictions
pr.boo = predict(boost.train,newdata=test_data,n.trees=bi)
solution <- data.frame(id = c(1:500), y = pr.boo)
names(solution)[2] <- "y"
names(solution)
nrow(solution)
write.csv(solution, file = "9_boosting.csv", row.names = FALSE)




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
