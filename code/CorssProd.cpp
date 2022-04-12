// [[Rcpp::depends(RcppArmadillo, RcppEigen)]]

#include <RcppArmadillo.h>
#include <RcppEigen.h>
using namespace Rcpp;

// [[Rcpp::export]]
SEXP MatMult(const Eigen::Map<Eigen::MatrixXd> A,
                      Eigen::Map<Eigen::MatrixXd> B, 
                      int n_cores){
        
        Eigen::setNbThreads(n_cores);
        Eigen::MatrixXd C = A * B;
        return Rcpp::wrap(C);
}