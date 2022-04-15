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


# Package loading
load_all("C:/Users/Eunchong Kang/Desktop/Spring 2022/BIOS 735/group_project/package/glmLogistic")
# Documenting if needed
setwd("C:/Users/Eunchong Kang/Desktop/Spring 2022/BIOS 735/group_project/package/glmLogistic")
document()


# Check package
check(manual=TRUE)
test_file("tests/testthat/test-glmLogistic.R")


# Check documenting
?loglik()
?d1.loglik()
?beta.updator()
?optim.irls()


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
optim.irls(X=X1, Y=Y1, beta=beta)


## Compare it to glm
glm(Y1 ~ X1-1, family = "binomial")


# From our data
#fit_irls <- optim.irls(X=X, Y=Y, beta=beta, tol=10^-5, maxit=50)

#vignette("datatable-intro", package = "data.table")
