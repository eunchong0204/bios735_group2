####################################################
##########              IRLS              ##########
####################################################

# Library Set-up
library(tidyverse)
library(optimx)
library(caret)
library(Rcpp)
library(RcppArmadillo)

# Working Directory
setwd("C:/Users/Eunchong Kang/Desktop/Spring 2022/BIOS 735/group_project/bios735_group2/code/IRLS")

# Rcpp functions
sourceCpp("IRLS_functions_v3.cpp")


########## Data Import and Transformation ##########

# set a randomization seed
set.seed(735)

# load heard disease data (add your own filepath to access the data)
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


## scale
heart.train$BMI <- scale(heart.train$BMI)
heart.train$PhysicalHealth <- scale(heart.train$PhysicalHealth)
heart.train$MentalHealth <- scale(heart.train$MentalHealth)
heart.train$SleepTime <- scale(heart.train$SleepTime)


# Design Matrix
Y <- model.matrix(~., data=heart.train)[,2]
X <- model.matrix(~., data=heart.train)[,-2]




########## Implementation ##########

# Setting
beta <- matrix(0, ncol = 1, nrow = ncol(X))

# Run
fit_irls <- optim_irls(X=X, Y=Y, beta=beta, tol=10^-5, maxit=50)

# Results
fit_irls
