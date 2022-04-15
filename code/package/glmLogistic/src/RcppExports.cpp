// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// d1_logli
arma::mat d1_logli(arma::mat X, arma::vec y, arma::vec beta);
RcppExport SEXP _glmLogistic_d1_logli(SEXP XSEXP, SEXP ySEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type y(ySEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(d1_logli(X, y, beta));
    return rcpp_result_gen;
END_RCPP
}
// logli
double logli(arma::mat X, arma::vec Y, arma::vec beta);
RcppExport SEXP _glmLogistic_logli(SEXP XSEXP, SEXP YSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(logli(X, Y, beta));
    return rcpp_result_gen;
END_RCPP
}
// beta_updator
DataFrame beta_updator(arma::mat X, arma::vec Y, arma::vec beta);
RcppExport SEXP _glmLogistic_beta_updator(SEXP XSEXP, SEXP YSEXP, SEXP betaSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    rcpp_result_gen = Rcpp::wrap(beta_updator(X, Y, beta));
    return rcpp_result_gen;
END_RCPP
}
// optim_irls
List optim_irls(arma::mat X, arma::vec Y, arma::vec beta, double tol, double maxit);
RcppExport SEXP _glmLogistic_optim_irls(SEXP XSEXP, SEXP YSEXP, SEXP betaSEXP, SEXP tolSEXP, SEXP maxitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type Y(YSEXP);
    Rcpp::traits::input_parameter< arma::vec >::type beta(betaSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< double >::type maxit(maxitSEXP);
    rcpp_result_gen = Rcpp::wrap(optim_irls(X, Y, beta, tol, maxit));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_glmLogistic_d1_logli", (DL_FUNC) &_glmLogistic_d1_logli, 3},
    {"_glmLogistic_logli", (DL_FUNC) &_glmLogistic_logli, 3},
    {"_glmLogistic_beta_updator", (DL_FUNC) &_glmLogistic_beta_updator, 3},
    {"_glmLogistic_optim_irls", (DL_FUNC) &_glmLogistic_optim_irls, 5},
    {NULL, NULL, 0}
};

RcppExport void R_init_glmLogistic(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
