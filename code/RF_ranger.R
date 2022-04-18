####################################################
##########             Random Forest            ##########
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



# Raondom Forest
tgrid <- expand.grid(
  .mtry = c(2,4,6,8,10,12),
  .splitrule = "gini",
  .min.node.size = c(10,20)
)
time_start <- Sys.time()
fit_RF1 <- train(HeartDisease ~ ., data = heart.train, 
#                tuneGrid = tgrid,
                method = "ranger",
                trControl = trainControl(method = "cv",number = 5,verboseIter = TRUE,classProbs = TRUE))
time_end <- Sys.time()

fit_RF

## Computing time
print(time_end - time_start)

## confusion Matrix
pred_RF <- predict(fit_RF, heart.test)
confusionMatrix(data = pred_RF, reference = heart.test$HeartDisease)

## ROC curve
library(pROC)
# ROC for training set
prob_t <- predict(fit_RF, heart.train[,-1],type = "prob")
roc <- roc(heart.train$HeartDisease,prob_t$Yes, plot = TRUE, print.auc = TRUE)

# ROC for testing set
prob <- predict(fit_RF, heart.test[,-1],type = "prob")
roc <- roc(heart.test$HeartDisease,prob$Yes, plot = TRUE, print.auc = TRUE)


save(fit_RF,file = "fit_RF_tune.Rdata")
