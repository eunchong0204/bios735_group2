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
?beta.updater()
?optim.IRLS()
?optim.BFGS()


#############################################
# Run our data

## Load data
#Eunchong
load("C:/Users/Eunchong Kang/Desktop/Spring 2022/BIOS 735/group_project/bios735_group2/data/HeartDataDerived.Rdata")

## Design Matrix
Y <- model.matrix(~., data=heart.train)[,2]
X <- model.matrix(~., data=heart.train)[,-2]

### The starting value 1
beta0 <- matrix(0, ncol = 1, nrow = ncol(X))

### The starting value 2
beta02 <- matrix(0.5, ncol = 1, nrow = ncol(X))


## optim.irls function
start = Sys.time() # record start time
fit_IRLS <- optim.IRLS(X=X, Y=Y, beta=beta0)
end = Sys.time()
print(end - start)

### does not converge
fit_IRLS2 <- optim.IRLS(X=X, Y=Y, beta=beta02)


## glm function
start = Sys.time()
fit_glm <- glm(Y ~ X-1, family = "binomial")
end = Sys.time()
print(end - start)


## optim.BFGS function
fit_BFGS <- optim.BFGS(X=X, Y=Y, beta=beta0)
fit_BFGS

### Converge
fit_BFGS2 <- optim.BFGS(X=X, Y=Y, beta=beta02)
fit_BFGS2



# Export to excel file
#library("openxlsx")

results <- data.frame("GLM"=fit_glm$coefficients, "IRLS"=fit_IRLS$Estimate$Beta, "BFGS"=fit_BFGS$Estimate$Beta)
row.names(results) <- names(fit_glm$coefficients)
results_rounded <- round(results, digits = 4)
results_rounded

max(results$GLM-results$IRLS)
max(results$GLM-results$BFGS)

#write.xlsx(results_rounded, sheetName="sheet1", file="result.xlsx", rowNames=TRUE)

