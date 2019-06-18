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

# 3) Some basic modelling

#GLM with no extra informations
basic_lm <- lm(y~ .,data=training_data) 
basic_lm$coefficients
summary(basic_lm)

#All significant predictors with no interactions
sig_lm1 <- lm(y~X1+X2+X3+X4+X8+X12+X13+X23+X24+X25, data = training_data)
summary(sig_lm1)

#Do not do this!!!!!!
#inter_sig_lm <- lm(y~X1*X2*X3*X4*X8*X12*X13*X23*X24*X25, data = training_data)
#summary(inter_sig_lm)

#ALL interactions
#basic_lm2 <- lm(y~.*.,data=training_data)
#basic_lm2$coefficients
#summary(basic_lm2)

#Significant predictors and interaction against categorical
sig_with_inter_lm <- lm(y~(X1+X2+X3+X4+X8+X13+X23+X24+X25)*X12, data = training_data)
summary(sig_with_inter_lm)
plot(sig_with_inter_lm$residuals)

#Final linear model
final_lm <- lm(y~X1+X2+(X3+X13)*X12+X4+X8+X23+X24+X25, data = training_data)
summary(final_lm)
plot(final_lm$residuals)


#Validation
#set.seed(1)
#test <- sample(c(1:500), size = 80)
#final_lm <- lm(y~(X1+X2+X3+X4+X5+X8+X13+X23+X25)*X12, data = training_data[-test,])
#predicted_y <- predict(final_lm, newdata = training_data[test,])
#mean_err <- mean((training_data[test,]$y-predicted_y)^2)
#mean_err





# 4) Predicting and Converting to CSV
#summary(x_test)
x_test = model.matrix(X1.500 ~ . ,test_data )[,-1]  
prediction <- predict(final_lm, newx = x_test)
solution <- data.frame(id = c(1:500), y = prediction)
names(solution)
nrow(solution)
write.csv(solution, file = "6_MLR_no_categorical_prediction_answer.csv", row.names = FALSE)
