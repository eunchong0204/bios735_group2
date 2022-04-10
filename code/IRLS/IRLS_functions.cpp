//[[Rcpp::depends("RcppArmadillo")]]

#define ARMA_USE_BLAS
#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat d1_logli(arma::mat X, arma::vec y, arma::vec beta){
        return X.t() * (y-(1-1/(1+exp(X*beta))));
}

// [[Rcpp::export]]
arma::vec logli(arma::mat X, arma::vec Y, arma::vec beta){
        return Y.t()*(X*beta)-sum(log(1+exp(X*beta)));
}

// [[Rcpp::export]]
arma::vec beta_calculator(arma::mat X, arma::vec Y, arma::vec beta){
        arma::vec exp_eta_t = exp(X*beta);
        arma::vec e = pow(1+exp_eta_t, 2)/exp_eta_t%(Y - exp_eta_t/(1+exp_eta_t));
        arma::vec w = exp_eta_t/pow(1+exp_eta_t, 2);
        arma::vec z = X*beta + e;
        return  inv(X.t() * (X.each_col() % w))* X.t() * (z.each_col() % w);
}