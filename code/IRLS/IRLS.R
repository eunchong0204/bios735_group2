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
#setwd("C:/Users/Eunchong Kang/Desktop/Spring 2022/BIOS 735/group_project")

# Rcpp functions
sourceCpp("IRLS_functions.cpp")


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




########## Data Import and Transformation ##########

# Create function
optim_irls <- function(X, Y, beta, logL, eps=Inf, tol=10^-5, maxit=50) {
        
        # store the number of samples
        n <- nrow(X)
        
        # Iteration
        iter <- 0
        
        while (eps > tol & iter < maxit) {
                # save the previous value
                logL0 <- logL
                beta0 <- beta
                
                # subsample
                index <- sample(1:n, size = n, replace = FALSE)
                
                # Calculate Beta(t+1)
                beta <- beta_calculator(X[index, ], Y[index], beta)
                
                
                # update the log likelihood
                logL <- logli(X = X, Y = Y, beta)
                
                # calculate the euclidean distance, could also use the log likelihood if we wanted
                #eps  = sqrt(sum((beta - beta0) ^ 2))
                eps <- abs(logL0 - logL) / abs(logL0)
                
                # update the iteration number
                iter <- iter + 1
                if (iter == maxit)
                        warning("Iteration limit reached without convergence")
                
                # print out info to keep track
                cat(sprintf("Iter: %d logL: %.2f beta0: %.3f beta1: %.3f beta2: %.3f eps:%f\n",
                                iter, logL, beta[1], beta[2], beta[3], eps))
        }
        
        #list_result <- list(beta, logL, iter, eps)
        #names(list_result) <- c("beta", "Log-likelihood", "iteration", "eps")
        return(list("beta"=beta, "Log-likelihood"=logL, "iteration"=iter, "eps"=eps))
}

# Setting
beta <- matrix(0, ncol = 1, nrow = ncol(X))
logL <- logli(X,Y,beta)

# Run
fit <- optim_irls(X=X, Y=Y, beta=beta, logL=logL)

# Results
fit

