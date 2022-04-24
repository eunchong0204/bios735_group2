####################################################
##########              SVM              ##########
####################################################
  
# load relevant packages
library(tidyverse)
library(optimx)
library(caret)
library(pROC)

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
SVMIndex <- c(sample(which(heart.train$HeartDisease == "No"), sum(heart.train$HeartDisease == "Yes")), which(heart.train$HeartDisease == "Yes"))
heart.train <- heart.train[SVMIndex,]
table(heart.train$HeartDisease)


# convert categorica data to dummy variables
dummies <- dummyVars(~.,data = heart.train[,c(3:5,8:14,16:18)],fullRank = TRUE)
c2 <-predict(dummies,heart.train[,c(3:5,8:14,16:18)])
X_train <- as.data.frame(cbind(heart.train[,c(2,6:7,15)],c2))
Y_train <- heart.train[,1]
#data_train <- as.data.frame(cbind(heart.train[,c(1:2,6:7,15)],c2))
table(Y_train)

dummies <- dummyVars(~.,data = heart.test[,c(3:5,8:14,16:18)],fullRank = TRUE)
c2 <-predict(dummies,heart.test[,c(3:5,8:14,16:18)])
X_test <- as.data.frame(cbind(heart.test[,c(2,6:7,15)],c2))
Y_test <- heart.test[,1]
#data_test <- as.data.frame(cbind(heart.test[,c(1:2,6:7,15)],c2))


## Using package caret

train_control <- trainControl(method="repeatedcv", number=5,classProbs = TRUE)


### Linear
fit_LSVM <- train(X_train,Y_train,
                  method = "svmLinear", 
                  trControl = train_control, 
                  tuneLength = 10,
                  preProcess = c("center","scale"))

fit_LSVM$results # summary of the model fitting

Var <- varImp(fit_LSVM) # variable importance
plot(varImp(object=fit_LSVM,scale=FALSE),top = 11,   main="Linear SVM - Variable Importance")


test_Lpred <- predict(fit_LSVM,newdata = X_test) # prediction for the testing data
confusionMatrix(table(test_Lpred,Y_test)) # compute sensitivity and specificity using the testing data

save(fit_LSVM,file = "fit_LSVM.Rdata")


prob_SVM <- predict(fit_LSVM, X_test,type = "prob") # ROC curve
roc_SVM <- roc(heart.test$HeartDisease,prob_SVM$Yes, plot = TRUE, print.auc = TRUE)


