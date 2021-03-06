% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor.cramer.R
\name{cor.cramer}
\alias{cor.cramer}
\title{Cramer's V}
\usage{
cor.cramer(x, correct = TRUE, tri = c("both", "lower", "upper"), digits = 2,
           as.na = NULL, check = TRUE, output = TRUE)
}
\arguments{
\item{x}{a matrix or data frame with integer vectors, character vectors or factors.}

\item{correct}{logical: if \code{TRUE} (default), the bias-corrected Cramer's V is computed.}

\item{tri}{a character string or character vector indicating which triangular of the matrix
to show on the console, i.e., \code{both} for upper and lower triangular,
\code{lower} (default) for the lower triangular, and \code{upper} for the upper
triangular.}

\item{digits}{an integer value indicating the number of decimal places digits to be used for
displaying Cramer's V.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before conducting the analysis.}

\item{check}{logical: if \code{TRUE}, argument specification is checked.}

\item{output}{logical: if \code{TRUE}, output is shown on the console.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following entries:
function call (\code{call}), type of analysis \code{type}, matrix or data frame specified in
\code{x} (\code{data}), specification of function arguments (\code{args}), and
list with results (\code{result}).
}
\description{
This function computes the (bias-corrected) Cramer's V between two or more than two variables.
}
\details{
Cramer's V can have large bias tending to overestimate the strength of association which depends
on the size of the table and the sample size. As proposed by Bergsma (2013) a bias correction can be
applied to obtain the bias-corrected Cramer's V.
}
\examples{
dat <- data.frame(x = c(1, 1, 2, 1, 3, 3, 2, 2, 1, 2),
                  y = c(1, 2, 2, 1, 3, 4, 1, 2, 3, 1),
                  z = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 2))

# Bias-corrected Cramer's V between x and y
cor.cramer(dat[, c("x", "y")])

# Cramer's V between x and y
cor.cramer(dat[, c("x", "y")], correct = FALSE)

# Bias-corrected Cramer's V matrix between x, y, and z
cor.cramer(dat[, c("x", "y", "z")])

# Cramer's V matrix between x, y, and z
cor.cramer(dat[, c("x", "y", "z")], correct = FALSE)
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
John Wiley & Sons.

Bergsma, W. (2013). A bias correction for Cramer's V and Tschuprow's T. \emph{Journal of the Korean
Statistical Society, 42}, 323-328. https://doi.org/10.1016/j.jkss.2012.10.002
}
\seealso{
\code{\link{cor.matrix}}, \code{\link{cor.cont}}, \code{\link{cor.phi}}, \code{\link{cor.poly}},
\code{\link{cohens.d}}.
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
