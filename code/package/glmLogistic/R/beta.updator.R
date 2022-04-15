#' Updating Parameter Values within IRLS
#'
#'
#' \code{beta.updator} is used to update parameter values during IRLS iteration for fitting the logistic regression model.
#' \code{beta.updator} takes a design matrix, a binary response vector, and a vector of the current parameters.
#'
#'
#' @usage beta.updator(X, Y, beta)
#'
#'
#' @param X a design matrix. \code{X} must be of numeric type.
#' @param Y a response vector. \code{Y} must consist of \code{1} and \code{0} with \code{1} representing the event.
#' The length of \code{Y} must be equal to the number of rows in \code{X}.
#' @param beta a vector of the parameter values. \code{beta} must be of numeric type.
#' The length of \code{beta} must be equal to the number of columns in \code{X}.
#'
#'
#' @return \code{beta.updator} returns a vector of the updated parameter values for IRLS method while fitting the logistic regression model.
#' The returned value has the same size as beta and is of numeric type.
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
#' ## Calculate the updated parameters
#' beta.updator(X=X, Y=Y, beta=beta)
#'
#' @importFrom Rcpp evalCpp
#' @export
beta.updator <- function(X, Y, beta){
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
        beta_updator(X, Y, beta)
}
