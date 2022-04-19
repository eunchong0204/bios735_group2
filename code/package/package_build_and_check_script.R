#Rscript for Group project

#library
library(usethis)
library(devtools)

# Unneccessary when executing
#library(Rcpp)
#library(RcppArmadillo)
#library(microbenchmark)
#library(MASS)

# Package set-up
## RcppArmadillo.package.skeleton(name="glmLogistic")
## use_test("glmLogistic")


## Di
# Package loading
#load_all("~/Documents/Courses in UNC/BIOS 735/bios735_group2/code/package/glmLogistic")
# Documenting if needed
#setwd("~/Documents/Courses in UNC/BIOS 735/bios735_group2/code/package/glmLogistic")


## Eunchong
# Package loading
load_all("C:/Users/Eunchong Kang/Desktop/Spring 2022/BIOS 735/group_project/bios735_group2/code/package/glmLogistic")
# Documenting if needed
setwd("C:/Users/Eunchong Kang/Desktop/Spring 2022/BIOS 735/group_project/bios735_group2/code/package/glmLogistic")
document()


# Check package
check(manual=TRUE)
test_file("tests/testthat/test-glmLogistic.R")


# Check documenting
?loglik()
?d1.loglik()
?beta.updator()
?optim.IRLS()
?optim.BFGS()


# Run a Example (Toy dataset)
## Create data
dt <- mtcars
Y1 <- dt$vs
X1 <- cbind(rep(1, 32), dt$mpg, dt$am)
beta = rep(0,3)

## Execute helper functions
loglik(X1,Y1,beta)
d1.loglik(X1, Y1, beta)
beta.updator(X1, Y1, beta)

## Execute optimizer
optim.IRLS(X=X1, Y=Y1, beta=beta)

## Compare it to glm
glm(Y1 ~ X1-1, family = "binomial")


#############################################
# Run our data

## Load data
#Eunchong
load("C:/Users/Eunchong Kang/Desktop/Spring 2022/BIOS 735/group_project/bios735_group2/data/HeartDataDerived.Rdata")

## Design Matrix
Y <- model.matrix(~., data=heart.train)[,2]
X <- model.matrix(~., data=heart.train)[,-2]
beta0 <- matrix(0, ncol = 1, nrow = ncol(X))

## optim.irls function
fit_IRLS <- optim.IRLS(X=X, Y=Y, beta=beta0)
fit_IRLS

fit_irls <- optim_irls(X=X, Y=Y, beta=beta0, tol = 10^-10)
fit_irls$Log_Likelihood

## glm function
fit_glm <- glm(Y ~ X-1, family = "binomial")
fit_glm$coefficients
logLik(fit_glm)

## optim.BFGS function
fit_BFGS <- optim.BFGS(X=X, Y=Y, beta=beta0, maxit=10000)
fit_BFGS

