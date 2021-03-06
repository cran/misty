% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor.cont.R
\name{cor.cont}
\alias{cor.cont}
\title{Pearson's Contingency Coefficient}
\usage{
cor.cont(x, adjust = FALSE, tri = c("both", "lower", "upper"), digits = 2,
         as.na = NULL, check = TRUE, output = TRUE)
}
\arguments{
\item{x}{a matrix or data frame with integer vectors, character vectors or factors..}

\item{adjust}{logical: if \code{TRUE}, the adjusted contingency coefficient (i.e., Sakoda's
adjusted Pearson's C) is computed.}

\item{tri}{a character string indicating which triangular of the matrix to show on the console,
i.e., \code{both} for upper and lower triangular, \code{lower} (default) for the lower
triangular, and \code{upper} for the upper triangular.}

\item{digits}{an integer value indicating the number of decimal places digits to be used for
displaying contingency coefficients.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before conducting the analysis.}

\item{check}{logical: if \code{TRUE}, argument specification is checked.}

\item{output}{logical: if \code{TRUE}, output is shown on the console.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following entries:
function call (\code{call}), type of analysis \code{type},  matrix or data frame specified in
\code{x} (\code{data}), specification of function arguments (\code{args}), and
list with results (\code{result}).
}
\description{
This function computes the (adjusted) Pearson's contingency coefficient between two or more than two variables.
}
\details{
The maximum contingency coefficient is determined by the distribution of the two variables, i.e., the
contingency coefficient cannot achieve the value of 1 in many cases. According to Sakoda (1977), the
contingency coefficient can be adjusted by relating the coefficient to the possible maximum, \eqn{C / C_max}.
}
\examples{
dat <- data.frame(x = c(1, 1, 2, 1, 3, 3, 2, 2, 1, 2),
                  y = c(3, 2, 3, 1, 2, 4, 1, 2, 3, 4),
                  z = c(2, 2, 2, 1, 2, 2, 1, 2, 1, 2))

# Contingency coefficient between x and y
cor.cont(dat[, c("x", "y")])

# Adjusted contingency coefficient between x and y
cor.cont(dat[, c("x", "y")], adjust = TRUE)

# Contingency coefficient matrix between x, y, and z
cor.cont(dat)

# Adjusted contingency coefficient matrix between x, y, and z
cor.cont(dat, adjust = TRUE)
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
John Wiley & Sons.

Sakoda, J.M. (1977). Measures of association for multivariate contingency tables. \emph{Proceedings of the
Social Statistics Section of the American Statistical Association (Part III)}, 777-780.
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}

\code{\link{cor.matrix}}, \code{\link{cor.cramer}}, \code{\link{cor.phi}}, \code{\link{cor.poly}}, \code{\link{cohens.d}}, .
}
