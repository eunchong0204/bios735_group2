#' Fitting a Logistic Regression
#'

#' \code{optim.IRLS} is used to fit a logistic regression model by giving a design matrix, a binary response vector, and an initial beta.
#' \code{optim.IRLS} uses Iterative Reweighted Least Sqaures (IRLS) to find the maximum likelihood estimates of the model.
#' A absolute change of log likelihood is used for a convergence criterion.
#' Tolerance for convergence and a maximum iteration limit can be adjusted by arguments.
#'
#'
#' @usage optim.IRLS(X, Y, beta, tol=10^-10, maxit=50)
#'
#'
#' @param X a design matrix. \code{X} must be of numeric type.
#' @param Y a response vector. \code{Y} must consist of \code{1} and \code{0} with \code{1} representing the event.
#' The length of \code{Y} must be equal to the number of rows in \code{X}.
#' @param beta an initial vector for the estimates of the parameters. \code{beta} must be of numeric type.
#' The length of \code{beta} must be equal to the number of columns in \code{X}.
#' @param tol tolerance for convergence. The default value is 10^-10.
#' @param maxit a maximum iteration limit. The default value is 50.
#'
#'
#' @return a list with the following elements:
#' \itemize{
#' \item{\code{Estimate: } a dataframe with the estimates of parameters (\code{beta})and the estimates of its standard errors (\code{se}).}
#' \item{\code{Log_Likelihood: } a value of the log likelihood.}
#' \item{\code{Iteration: } the number of iterations executed for convergence.}
#' \item{\code{eps: } the last relative change of log likelihood.}
#' }
#'
#' @examples
#' ## Creating data from mtcars
#' dt <- mtcars
#' # Response: vs variable
#' Y <- dt$vs
#' # Covariates: mpg and am variables
#' X <- cbind(rep(1, 32), dt$mpg, dt$am)
#' # Initial beta
#' beta = rep(0,3)
#'
#' ## Fit the model
#' optim.IRLS(X=X, Y=Y, beta=beta)
#'
#'
#' @importFrom Rcpp evalCpp
#' @export
optim.IRLS <- function(X, Y, beta, tol=10^-10, maxit=50){
        # Check type of X, Y, and beta
        if (typeof(X) != "double" |  typeof(Y) != "double" | typeof(X) != "double"){
                stop("At least one of input is not of numeric")
        }

        # Check compatibility in matrix calculation
        if (nrow(X) != length(Y) |  ncol(X) != length(beta)){
                stop("Matrices are not compatible.")
        }

        # Check Response variable
        if (!identical(sort(unique(Y)), c(0,1))){
                warning("Y vector does not consist of 0 and 1")
        }

        # Check Returned Value
        value <- optim_irls(X=X, Y=Y, beta=beta, tol=tol, maxit=maxit)
        if (sum(is.nan(value[[1]][,1])) != 0){
                warning("Algorithm did not converge! Change the starting beta")
        } else{
                return(value)
        }
}
