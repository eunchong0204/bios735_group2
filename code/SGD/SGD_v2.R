####################################################
##########              SGD              ###########
####################################################

# Library Set-up
library(tidyverse)
library(optimx)
library(caret)
library(Rcpp)
library(RcppArmadillo)

# Working Directory
setwd("C:/Users/Eunchong Kang/Desktop/Spring 2022/BIOS 735/group_project/bios735_group2/code/SGD")

# Rcpp functions
sourceCpp("GD_functions.cpp")

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
optim_sgd <-function(Y, X, beta, alpha, ratio, cf, tol=10^-8, eps=100000, maxit=100000) {
        
        # iter
        iter <- 1
        
        # epscount
        epscount <- 0
                
        # #of samples
        n <- nrow(X)
        
        # create logL vectors
        logL = rep(NA, maxit)
        
        while (eps > tol & iter < maxit & epscount < 4) {
                        
                # take subsample
                index = sample(1:n, size = ratio * n, replace = FALSE)
                        
                # calculate h, the increment
                h =  alpha * d1_logli(X = X[index, ], Y = Y[index], beta = beta)
                        
                # update lambda
                beta = beta + h
                        
                # update the log likelihood  Loglike <- function (X, Y, beta)
                logL[iter] = logli(X = X, Y = Y, beta = beta)
                        
                # use relative change in logL from 1000 iterations prior
                # this is because randomness between single iterations large, smooths out
                if (iter > cf) {
                        eps  = abs(logL[iter] - logL[iter - cf]) / abs(logL[iter - cf])
                }
                        
                # we use this count to protect against randomly hitting the convergene limit early
                if (eps < tol) {
                        epscount = epscount + 1
                }
                        
                # update the iteration number
                iter = iter + 1
                
                # terminate if iter hits maxit
                if (iter == maxit) {
                        warning("Iteration limit reached without convergence")
                }
                        
                # print out info to keep track
                if (floor(iter / cf) == ceiling(iter / cf)) {
                        cat(sprintf("Iter: %d logL: %.4f beta0: %.5f beta1: %.5f beta2: %.5f etc. eps:%.10f\n",
                                                iter, logL[iter - 1], beta[1], beta[2], beta[3], eps))
                }
        }
        
        return(list("beta"=beta, "Log-likelihood"=logL, "iteration"=iter, "eps"=eps))
        
}

# Setting
beta <- matrix(0, ncol = 1, nrow = ncol(X))

# Run
# .. mins
start = Sys.time()
fit_sgd <- optim_sgd(Y=Y, X=X, beta=beta, alpha=0.00001, ratio=1, cf=1, tol=10^-8)
end = Sys.time()
print(end - start)

# Results
fit_sgd$beta
