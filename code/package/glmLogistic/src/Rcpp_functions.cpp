#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat d1_logli(arma::mat X, arma::vec y, arma::vec beta){
        // First derivative of log-likelihood: t(X) * (Y- pi)
        // pi = e^(X*beta)/(1+e^(X*beta))
        return X.t() * (y-(1-1/(1+exp(X*beta))));
}

// [[Rcpp::export]]
double logli(arma::mat X, arma::vec Y, arma::vec beta){
        // log-likelihood: sum(y_i * log(pi/(1-pi)) + log(1-pi))
        // pi = e^(X*beta)/(1+e^(X*beta))
        return arma::as_scalar(Y.t()*(X*beta)-sum(log(1+exp(X*beta))));
}

// [[Rcpp::export]]
DataFrame beta_updater(arma::mat X, arma::vec Y, arma::vec beta){
        // calculate e^(X*beta)
        arma::vec exp_eta_t = exp(X*beta);

        // e = (1+e^(X*beta))^2/e^(X*beta) * (y - e^(X*beta)/(1+e^(X*beta)))
        arma::vec e = pow(1+exp_eta_t, 2)/exp_eta_t%(Y - exp_eta_t/(1+exp_eta_t));

        // w = e^(X*beta)/(1+e^(X*beta))^2
        arma::vec w = exp_eta_t/pow(1+exp_eta_t, 2);

        // z = X*beta + e
        arma::vec z = X*beta + e;

        // (t(X)*W*X)^-1
        arma::mat cov = inv(X.t() * (X.each_col() % w));

        // beta^(t+1) = (t(X)*W*X)^-1 * t(X) * W * z
        arma::vec beta_update = cov * X.t() * (z.each_col() % w);

        // return updated beta and its se
        return DataFrame::create(Named("Estimate")=beta_update,
                            Named("se")=arma::sqrt(cov.diag()));
}

// [[Rcpp::export]]
List optim_irls(arma::mat X,
                arma::vec Y,
                arma::vec beta,
                double tol=10^-10,
                double maxit=50) {

        // Iteration setting
        int iter = 0;

        // Set initial log-likelihood with the initial beta
        double logL = logli(X=X, Y=Y, beta);

        // Set eps to infinity
        double eps=std::numeric_limits<double>::infinity();

        // Create an empty vector for se
        arma::vec se(size(beta));

        while (eps > tol && iter < maxit) {
                // save the previous value
                double logL0 = logL;

                // Calculate Beta(t+1) and se
                DataFrame update = beta_updater(X, Y, beta);
                beta = as<arma::vec>(update["Estimate"]);
                se = as<arma::vec>(update["se"]);

                // update the log likelihood
                logL = logli(X = X, Y = Y, beta);

                // calculate the absolute change of log likelihood
                eps = std::abs(logL0 - logL);

                // update the iteration number
                iter = iter + 1;

                // terminate if iter hits maxit
                if (iter == maxit)
                        warning("Iteration limit reached without convergence");

                // print out info to keep track
                Rprintf("Iter: %d logL: %.2f beta0: %.3f etc. eps:%.8f\n",
                        iter, logL, beta(0), eps);
        }

        //return beta, se, log-likelihood, iteration, and eps
        return List::create(Named("Estimate")=DataFrame::create(Named("Beta")=beta, Named("se")=se), Named("Log_Likelihood")=logL, Named("Iteration")=iter, Named("eps")=eps);
}





// [[Rcpp::export]]
List optim_BFGS(arma::mat X,
                arma::vec Y,
                arma::vec beta,
                double tol=10^-5,
                double maxit=1000) {

        // Iteration setting
        int iter = 0;

        // Set the initial value
        arma::vec xk = beta;


        // Define the identity matrix
        arma::mat I = arma::eye<arma::mat>(xk.n_rows, xk.n_rows);

        // Set the initial H matrix
        arma::mat Hk = I;

        // Set res to infinity
        double res = std::numeric_limits<double>::infinity();


        while (res > tol && iter <= maxit) {
                // save the previous value
                arma::mat gk = -d1_logli(X,Y,xk);

                // calculate the new direction of descent
                arma::mat dk = -Hk*gk;

                // Line Search with Wolfe condition


                // set rho
                double rho = 0.0001;

                // set sigma
                double sigma = 0.9;

                // set initial step length
                double ak = 1;
                double a = 0;
                double b = 12345;
                // Object function at current value
                double f = -logli(X,Y,xk);

                while(a < b){
                        // if not satisfy Armijo Condition
                        if(-logli(X,Y,(xk + ak*dk)) > f + rho*ak* as_scalar(gk.t()*dk)){

                                b = ak;

                                ak = 0.5*(a+b);

                                // if not satisfy Wolfe condition

                        }else if( as_scalar(-d1_logli(X,Y,(xk + ak*dk)).t()*dk) < sigma* as_scalar(gk.t()*dk)){
                                a = ak;
                                if(b == 12345){
                                        ak = 2*a;
                                }
                                else{

                                        ak = 0.5*(a+b);
                                }
                        }else{break;}
                }

                // then ak is the step length

                // update xk
                arma::vec x_next = xk + ak*dk;

                // update sk
                arma::vec sk = x_next-xk;

                // update yk
                arma::vec yk = -d1_logli(X,Y,x_next) + d1_logli(X,Y,xk);

                // update Hk
                Hk = (I-(sk*yk.t())/ as_scalar(sk.t()*yk)) * Hk * (I-yk*sk.t()/as_scalar(sk.t()*yk)) +
                        sk*sk.t()/as_scalar(sk.t()*yk);

                // update iteration
                iter = iter + 1;

                // update fun
                double f_next = -logli(X,Y,x_next);

                //calculate residual
                res = std::abs(f-f_next);

                // update current value
                xk = x_next;

                // update gradient
                gk = -d1_logli(X,Y,xk);

                // terminate if iter hits maxit
                if (iter == maxit)
                        warning("Iteration limit reached without convergence");

                // print out info to keep track
                Rprintf("Iter: %d ak: %8f Loglik: %.8f norm(gk): %.8f etc. eps:%.8f\n",
                        iter,ak,f_next, norm(gk), res);
        }
        // calculate se of beta

        // calculate e^(X*beta)
        arma::vec es = exp(X*xk);

        // w = e^(X*beta)/(1+e^(X*beta))^2
        arma::vec w = es/pow(1+es, 2);

        // (t(X)*W*X)^-1
        arma::mat cov = inv(X.t() * (X.each_col() % w));

        arma::vec se =  arma::sqrt(cov.diag());

        //return beta, se, log-likelihood, iteration, and eps
        return List::create(Named("Estimate")=DataFrame::create(Named("Beta")=xk, Named("se")=se), Named("Function_min")=logli(X,Y,xk), Named("Iteration")=iter, Named("eps")=res);
}
