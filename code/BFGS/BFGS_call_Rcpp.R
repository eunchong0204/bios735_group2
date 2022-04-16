# BFGS call rcpp
sourceCpp("~/Documents/Courses in UNC/BIOS 735/bios735_group2/code/BFGS/BFGS.cpp")

# Design Matrix
X <- model.matrix(~.,heart.train[,-1])
# Response Matrix
Y <- subset.matrix(model.matrix(~HeartDisease, heart.train),select = 2)
# Initial Value
beta0 <- matrix(0, ncol = 1, nrow = ncol(X))
beta_se(X,Y,beta0)

start1 <- Sys.time()
BFGS_optim(beta = beta0,
           X = X,
           Y = Y)
end1 <- Sys.time()
end1-start1 # Time difference of 40.53283 secs