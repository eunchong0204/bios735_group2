//[[Rcpp::depends("RcppArmadillo")]]

#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::mat d1_logli(arma::mat X, arma::vec Y, arma::vec beta){
        return X.t() * (Y-(1-1/(1+exp(X*beta))));
}

// [[Rcpp::export]]
arma::vec logli(arma::mat X, arma::vec Y, arma::vec beta){
        return Y.t()*(X*beta)-sum(log(1+exp(X*beta)));
}