% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/size.prop.R
\name{size.prop}
\alias{size.prop}
\title{Sample Size Determination for Testing Proportions}
\usage{
size.prop(pi = 0.5, delta, sample = c("two.sample", "one.sample"),
          alternative = c("two.sided", "less", "greater"),
          alpha = 0.05, beta = 0.1, correct = FALSE,
          check = TRUE, output = TRUE)
}
\arguments{
\item{pi}{a number indicating the true value of the probability under the null hypothesis (one-sample test), \eqn{\pi}.0
or a number indicating the true value of the probability in group 1 (two-sample test), \eqn{\pi}.1.}

\item{delta}{minimum difference to be detected, \eqn{\delta}.}

\item{sample}{a character string specifying one- or two-sample proportion test,
must be one of "two.sample" (default) or "one.sample".}

\item{alternative}{a character string specifying the alternative hypothesis,
must be one of \code{"two.sided"} (default), \code{"less"} or \code{"greater"}.}

\item{alpha}{type-I-risk, \eqn{\alpha}.}

\item{beta}{type-II-risk, \eqn{\beta}.}

\item{correct}{a logical indicating whether continuity correction should be applied.}

\item{check}{logical: if \code{TRUE}, argument specification is checked.}

\item{output}{logical: if \code{TRUE}, output is shown.}
}
\value{
Returns an object of class \code{misty.object} with following entries:

\tabular{ll}{
  \code{call}      \tab function call \cr
  \code{type}      \tab type of the test (i.e., proportion) \cr
  \code{args}      \tab specification of function arguments \cr
  \code{result}       \tab list with the result, i.e., optimal sample size \cr
}
}
\description{
This function performs sample size computation for the one-sample and two-sample test for proportions
based on precision requirements (i.e., type-I-risk, type-II-risk and an effect size).
}
\examples{
#--------------------------------------
# Two-sided one-sample test
# H0: pi = 0.5, H1: pi != 0.5
# alpha = 0.05, beta = 0.2, delta = 0.2

size.prop(pi = 0.5, delta = 0.2, sample = "one.sample",
          alternative = "two.sided", alpha = 0.05, beta = 0.2)

#--------------------------------------
# One-sided one-sample test
# H0: pi <= 0.5, H1: pi > 0.5
# alpha = 0.05, beta = 0.2, delta = 0.2

size.prop(pi = 0.5, delta = 0.2, sample = "one.sample",
          alternative = "less", alpha = 0.05, beta = 0.2)

#--------------------------------------
# Two-sided two-sample test
# H0: pi.1 = pi.2 = 0.5, H1: pi.1 != pi.2
# alpha = 0.01, beta = 0.1, delta = 0.2

size.prop(pi = 0.5, delta = 0.2, sample = "two.sample",
          alternative = "two.sided", alpha = 0.01, beta = 0.1)

#--------------------------------------
# One-sided two-sample test
# H0: pi.1 <=  pi.1 = 0.5, H1: pi.1 > pi.2
# alpha = 0.01, beta = 0.1, delta = 0.2

size.prop(pi = 0.5, delta = 0.2, sample = "two.sample",
          alternative = "greater", alpha = 0.01, beta = 0.1)
}
\references{
Fleiss, J. L., Levin, B., & Paik, M. C. (2003). \emph{Statistical methods for rates and proportions} (3rd ed.).
John Wiley & Sons.

Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
John Wiley & Sons.

Rasch, D., Pilz, J., Verdooren, L. R., & Gebhardt, G. (2011).
\emph{Optimal experimental design with R}. Chapman & Hall/CRC.
}
\seealso{
\code{\link{size.mean}}, \code{\link{size.cor}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at},
}
