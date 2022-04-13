### BFGS Optimization Algorithm
### BIOS 735 Group 2

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

# Using d1_logli wrote by Eunchong
grad <- function(beta){
        return(-d1_logli(X,Y,beta))
}
fun <- function(beta){
        return(-Loglike(X,Y,beta))
}



# BFGS Algorithm with Wolfe line Search

# Similar as BFGS_Armijo except for the line search method.

BFGS <- function (x0, # Start Point
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
                
                # Line Search with Wolfe Condition 
                rho <- 0.0001
                sigma <- 0.9
                ak <- 1
                a <- 0
                b <- Inf
                f <- fun(xk)
                while (a < b){
                        if (fun(xk + ak*dk) > f + rho*ak*t(gk)%*%dk){
                                b <- ak
                                ak <- 0.5*(a+b)
                        }else if(t(grad(xk + ak*dk))%*%dk < sigma*t(gk)%*%dk){
                                a <- ak
                                if(b == Inf){ ak <- 2*a}
                                else{
                                        ak <- 0.5*(a+b)
                                }
                        }else {break}
                }

                # Get the step length ak
                
                x_next <- xk+ak*dk
                sk <- x_next-xk
                yk = grad(x_next)-grad(xk)
                Hk = Hk_f(sk,yk,Hk) # Using function Hk
                # Hk = (I-(sk%*%t(yk))/as.numeric(t(sk)%*%yk))*Hk*(I-yk%*%t(sk)/as.numeric(t(sk)%*%yk)) +
                #         sk%*%t(sk)/as.numeric(t(sk)%*%yk)
                k = k+1
                res = abs(fun(xk)-fun(x_next))
                # res = norm(gk,"2")
                xk = x_next
                gk = grad(xk)
                cat(sprintf("Iter: %d ak: %f fun: %f norm(gk): %f eps:%f\n",k, ak,fun(xk),norm(gk) ,res))
        }
        x_star <- xk
        fun.opt <- fun(xk)
        grad.opt <- grad(xk)
        return(list("x.opt"=x_star,"fun.opt"=fun.opt))
}
# A test



X <- model.matrix(~.,heart.train[,-1])
ncol(X)
Y <- subset.matrix(model.matrix(~HeartDisease, heart.train),select = 2)
beta0 <- matrix(0, ncol = 1, nrow = ncol(X))




start <- Sys.time()
BFGS(x0 = beta0, fun = fun, grad = grad, eps =  10^(-5), maxit=20000)
end <- Sys.time()

# [1] 57980.57

print(end-start)

