#' Finding a Value of the First Derivative of the Log-Likelihood
#'
#'
#' \code{d1.loglik} is used to find a value of the first derivative of the log-likelihood for the logistic regression model.
#' \code{d1.loglik} takes a design matrix, a binary response vector, and a vector of the parameters.
#'
#'
#' @usage d1.loglik(X, Y, beta)
#'
#'
#' @param X a design matrix. \code{X} must be of numeric type.
#' @param Y a response vector. \code{Y} must consist of \code{1} and \code{0} with \code{1} representing the event.
#' The length of \code{Y} must be equal to the number of rows in \code{X}.
#' @param beta a vector of the parameter values. \code{beta} must be of numeric type.
#' The length of \code{beta} must be equal to the number of columns in \code{X}.
#'
#'
#' @return \code{d1.loglik} returns a value of the first derivative of the log likelihood for the logistic regression model
#' The returned value is of numeric type
#'
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
#' ## Calculate the first derivative of log likelihood
#' d1.loglik(X=X, Y=Y, beta=beta)
#'
#'
#' @useDynLib glmLogistic
#' @importFrom Rcpp evalCpp
#' @export
d1.loglik <- function(X, Y, beta){
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

        # function
        d1_logli(X, Y, beta)
}
