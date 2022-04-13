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
arma::mat Hk_f(arma::mat sk, arma::mat yk, arma::mat Hk){
        int n = Hk.n_rows;
        arma::mat I = arma::eye<arma::mat>(n, n);
        return  (I-(sk*yk.t())/ as_scalar(sk.t()*yk)) * Hk * (I-yk*sk.t()/as_scalar(sk.t()*yk)) +
                sk*sk.t()/as_scalar(sk.t()*yk);
}

