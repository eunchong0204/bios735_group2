% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/optim.IRLS.R
\name{optim.IRLS}
\alias{optim.IRLS}
\title{Fitting a Logistic Regression}
\usage{
optim.IRLS(X, Y, beta, tol=10^-10, maxit=50)
}
\arguments{
\item{X}{a design matrix. \code{X} must be of numeric type.}

\item{Y}{a response vector. \code{Y} must consist of \code{1} and \code{0} with \code{1} representing the event.
The length of \code{Y} must be equal to the number of rows in \code{X}.}

\item{beta}{an initial vector for the estimates of the parameters. \code{beta} must be of numeric type.
The length of \code{beta} must be equal to the number of columns in \code{X}.}

\item{tol}{tolerance for convergence. The default value is 10^-10.}

\item{maxit}{a maximum iteration limit. The default value is 50.}
}
\value{
a list with the following elements:
\itemize{
\item{\code{Estimate: } a dataframe with the estimates of parameters (\code{beta})and the estimates of its standard errors (\code{se}).}
\item{\code{Log_Likelihood: } a value of the log likelihood.}
\item{\code{Iteration: } the number of iterations iterated for convergence.}
\item{\code{eps: } the last relative absolute change of log likelihood.}
}
}
\description{
\code{optim.IRLS} is used to fit a logistic regression model by giving a design matrix, a binary response vector, and an initial beta.
\code{optim.IRLS} uses Iterative Reweighted Least Sqaures (IRLS) to find the maximum likelihood estimates of the model.
A absolute change of log likelihood is used for a convergence criterion.
Tolerance for convergence and a maximum iteration limit can be adjusted by arguments.
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

## Fit the model
optim.IRLS(X=X, Y=Y, beta=beta)


}
