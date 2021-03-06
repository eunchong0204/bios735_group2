% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loglik.R
\name{loglik}
\alias{loglik}
\title{Finding Log-Likelihood}
\usage{
loglik(X, Y, beta)
}
\arguments{
\item{X}{a design matrix. \code{X} must be of numeric type.}

\item{Y}{a response vector. \code{Y} must consist of \code{1} and \code{0} with \code{1} representing the event.
The length of \code{Y} must be equal to the number of rows in \code{X}.}

\item{beta}{a vector of the parameter values. \code{beta} must be of numeric type.
The length of \code{beta} must be equal to the number of columns in \code{X}.}
}
\value{
\code{loglik} returns a value of the log likelihood for the logistic regression model in numeric type
}
\description{
\code{loglik} is used to find a value of the log-likelihood for the logistic regression model.
\code{loglik} takes a design matrix, a binary response vector, and a vector of the parameters.
}
\examples{
## Creating data from mtcars
dt <- mtcars
# Response: vs variable
Y <- dt$vs
# Covariates: mpg and am variables
X <- cbind(rep(1, 32), dt$mpg, dt$am)
# Initial beta
beta = rep(0,3)

## Calculate log likelihood
loglik(X=X, Y=Y, beta=beta)

}
