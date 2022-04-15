//[[Rcpp::depends("RcppArmadillo")]]

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
DataFrame beta_updator(arma::mat X, arma::vec Y, arma::vec beta){
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
                double tol=10^-5,
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
                DataFrame update = beta_updator(X, Y, beta);
                beta = as<arma::vec>(update["Estimate"]);
                se = as<arma::vec>(update["se"]);

                // update the log likelihood
                logL = logli(X = X, Y = Y, beta);

                // calculate the relative change of log likelihood
                eps = abs(logL0 - logL) / abs(logL0);

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
