# Helper functions

# Log-likelihood Function
Loglike <- function (X, Y, beta){
        loglike <- sum(Y*X%*%beta-log(1+exp(X%*%beta)))
        return(loglike)
}

# 1st-gradient of log-likelihood function

DLoglike <- function (X, Y, beta){
        d1 <- matrix(colSums(as.numeric(Y-exp(X%*%beta)/(1+exp(X%*%beta)))*X),ncol = 1)

        return(d1)
}

# Since BFGS is a optimization algorithm, we need to minimize negative log-likelihood function

grad <- function(beta){
        return(-DLoglike(X,Y,beta))
}
fun <- function(beta){
        return(-Loglike(X,Y,beta))
}

# BFGS Algorithm with Armijo line Search
BFGS_Armijo <- function (x0, # Start Point
                         fun, # Objective Function
                         grad, # 1-st Gradient
                         eps, # Tolerance
                         maxit=1000 # Maximum iteration times
                         ){
        k<-0
        xk <- x0
        I <- diag(rep(1,length(x0))) # Identity Matrix
        gk <- grad(x0) 
        Hk <- I # Start of Hk
        res = norm(gk) # Start Point of residual
         while (res > eps & k <= maxit){

                gk <- grad(xk)
                dk <- -Hk%*%gk # Direction of Descent
        
                # Armijo line search (To find a proper step length)
                
                ak <-1
                c <- 0.01
                f0 <- fun(xk)
                f1 <- fun(xk+ak*dk)
                slope <- t(gk)%*%dk
                while(f1 > f0 + c*ak*slope){
                        ak <- ak/2
                        f1 <- f1 <- fun(xk+ak*dk)
                }
                # The outputed ak is the chosen step length
                # Get the step length ak
                
                x_next <- xk+ak*dk
                sk <- x_next-xk
                yk = grad(x_next)-grad(xk)
                # Hk is the updated approximate of (Hessian)^-1
                Hk = (I-(sk%*%t(yk))/as.numeric(t(sk)%*%yk))*Hk*(I-yk%*%t(sk)/as.numeric(t(sk)%*%yk))+sk%*%t(sk)/as.numeric(t(sk)%*%yk)
                k = k+1
                res = abs(fun(xk)-fun(x_next))
                # res = norm(gk)
                xk = x_next
                gk = grad(xk)
                cat(sprintf("Iter: %d ak: %f logL: %f norm(gk): %f eps:%f\n",k, ak,fun(xk),norm(gk) ,res))
        }
        x_star <- xk
            return(x_star)
}

# BFGS Algorithm with Frank-Wolfe line Search

# Similar as BFGS_Armijo except for the line search method.

BFGS_FW <- function (x0, fun, grad, eps, maxit=1000){
        k<-0
        xk <- x0
        I <- diag(rep(1,length(x0)))
        gk <- grad(x0)
        Hk <- I
        res = norm(gk)
        while (res > eps & k <= maxit){
                
                gk <- grad(xk)
                dk <- -Hk%*%gk
                
                # Frank-Wolfe Line Search

                rho <- 0.0001
                sigma <- 0.9
                ak <- 1
                a <- 0
                b <- 1
                f <- fun(xk)
                while (a < b){
                        if (fun(xk + ak*dk) <= f + rho*ak*t(gk)%*%dk){
                                if(t(grad(xk + ak*dk))%*%dk >= sigma*t(gk)%*%dk){
                                        break
                                }else{
                                        a <- ak
                                        ak <- 0.5*(a+b)
                                }
                        }else{
                                b <- ak
                                ak <- (a+ak)/2
                        }
                }
             
                # Get the step length ak
                
                x_next <- xk+ak*dk
                sk <- x_next-xk
                yk = grad(x_next)-grad(xk)
                Hk = (I-(sk%*%t(yk))/as.numeric(t(sk)%*%yk))*Hk*(I-yk%*%t(sk)/as.numeric(t(sk)%*%yk))+sk%*%t(sk)/as.numeric(t(sk)%*%yk)
                k = k+1
                res = abs(fun(xk)-fun(x_next))
                # res = norm(gk)
                xk = x_next
                gk = grad(xk)
                cat(sprintf("Iter: %d ak: %f logL: %f norm(gk): %f eps:%f\n",k, ak,fun(xk),norm(gk) ,res))
        }
        x_star <- xk
        return(x_star)
}
# A test



X <- model.matrix(~.,heart.train[,-1])
ncol(X)
Y <- subset.matrix(model.matrix(~HeartDisease, heart.train),select = 2)
beta0 <- matrix(0, ncol = 1, nrow = ncol(X))

start1 <- Sys.time()
BFGS_Armijo(x0 = beta0, fun = fun, grad = grad, eps =  10^(-5), maxit=20000)
end1 <- Sys.time()
print(end1-start1)


start2 <- Sys.time()
BFGS_FW(x0 = beta0, fun = fun, grad = grad, eps =  10^(-5), maxit=20000)
end2 <- Sys.time()


print(end2-start2)

