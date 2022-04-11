####################################################
##########              SVM              ##########
####################################################
  
# load relevant packages
library(tidyverse)
library(optimx)
library(caret)


# set a randomization seed
set.seed(735)

# load heard disease data
setwd('D:/735/Project') #file path for Wanting
heart.data <- read.csv("heart_2020_cleaned.csv") 


## data pre-processing
# set factors as factors
heart.data$HeartDisease <- as.factor(heart.data$HeartDisease)
heart.data$Smoking <- as.factor(heart.data$Smoking)
heart.data$AlcoholDrinking <- as.factor(heart.data$AlcoholDrinking)
heart.data$Stroke <- as.factor(heart.data$Stroke)
heart.data$DiffWalking <- as.factor(heart.data$DiffWalking)
heart.data$Sex <- as.factor(heart.data$Sex)
heart.data$AgeCategory <- as.factor(heart.data$AgeCategory)
heart.data$Race <- as.factor(heart.data$Race)
heart.data$Diabetic <- as.factor(heart.data$Diabetic)
heart.data$PhysicalActivity <- as.factor(heart.data$PhysicalActivity)
heart.data$GenHealth <- as.factor(heart.data$GenHealth)
heart.data$Asthma <- as.factor(heart.data$Asthma)
heart.data$KidneyDisease <- as.factor(heart.data$KidneyDisease)
heart.data$SkinCancer <- as.factor(heart.data$SkinCancer)
# set numerics as numeric
heart.data$BMI <- as.numeric(heart.data$BMI)
heart.data$PhysicalHealth <- as.numeric(heart.data$PhysicalHealth)
heart.data$MentalHealth <- as.numeric(heart.data$MentalHealth)
heart.data$SleepTime <- as.numeric(heart.data$SleepTime)


# create training data index
trainIndex <- createDataPartition(heart.data$HeartDisease, p = .8, list = FALSE, times = 1)
# train set
heart.train <- heart.data[trainIndex,]
# test set
heart.test <- heart.data[-trainIndex,]

# check that proportion of positive/negative cases are equal
table(heart.train$HeartDisease) # 0.08559747 prop of positive cases
table(heart.test$HeartDisease) # 0.08558742 prop of positive cases



# functions for SVM algorithm
# convert categorica data to dummy variables
dummies <- dummyVars(~.,data = heart.train[,c(3:5,8:14,16:18)],fullRank = TRUE)
c2 <-predict(dummies,heart.train[,c(3:5,8:14,16:18)])
X_train <- as.data.frame(cbind(heart.train[,c(2,6:7,15)],c2))
Y_train <- heart.train[,1]
data_train <- as.data.frame(cbind(heart.train[,c(1:2,6:7,15)],c2))


dummies <- dummyVars(~.,data = heart.test[,c(3:5,8:14,16:18)],fullRank = TRUE)
c2 <-predict(dummies,heart.test[,c(3:5,8:14,16:18)])
X_test <- as.data.frame(cbind(heart.test[,c(2,6:7,15)],c2))
Y_test <- heart.test[,1]
data_test <- as.data.frame(cbind(heart.test[,c(1:2,6:7,15)],c2))


## Using package caret
### Linear
fit_LSVM <- train(X_train,Y_train,method = "svmLInear")
fit_LSVM$results # summary of the model fitting
#ggplot(fit_LSVM,metrix = "Kappa")
varImp(fitLSVM) # variable importance

test_Lpred <- predict(fit_LSVM,newdata = X_test) # prediction for the testing data
confusionMatrix(table(test_Lpred,Y_test)) # compute sensitivity and specificity using the testing data

### Radial
fit_RSVM <- train(X_train,Y_train,method = "svmRadial")
fit_RSVM$results # summary of the model fitting
#ggplot(fit_RSVM,metrix = "Kappa")
varImp(fit_RSVM) # variable importance

test_Rpred <- predict(fit_RSVM,newdata = X_test) # prediction for the testing data
confusionMatrix(table(test_Rpred,Y_test)) # compute sensitivity and specificity using the testing data





## method 2, Using package e1071, function svm
library(e1071)
gammalist <- c(0.005,0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05)
gammalist <- c(0.01,0.02,0.03,0.04,0.05)
tune.out <- tune.svm( as.factor(HeartDisease) ~., data = data_train[1:1000,], 
                      kernel='radial', cost=2^(-1:5), gamma = gammalist)
summary(tune.out)
summary(tune.out$best.model)
svm1 <- predict(tune.out$best.model, data_test[,-1])
confusionMatrix(svm1, as.factor(data_test$HeartDisease))



