% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diag.out.R
\name{check.outlier}
\alias{check.outlier}
\title{Statistical Measures for Leverage, Distance, and Influence}
\usage{
check.outlier(model, check = TRUE, ...)
}
\arguments{
\item{model}{a fitted model of class \code{"lm"}.}

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}

\item{...}{further arguments to be passed to or from methods.}
}
\value{
Returns a data frame with following entries:
\item{\code{idout}}{ID variable}
\item{\code{mahal}}{Mahalanobis distance}
\item{\code{hat}}{hat values}
\item{\code{rstand}}{standardized leverage-corrected residuals}
\item{\code{rstud}}{studentized leverage-corrected residuals}
\item{\code{cook}}{Cook`s distance}
\item{\code{Intercept.dfb}}{DFBetas for the intercept}
\item{\code{pred1.dfb}}{DFBetas for the slope of the predictor pred1}
\item{\code{....dfb}}{DFBetas for the slope of the predictor ...}
}
\description{
This function computes statistical measures for leverage, distance, and
influence for linear models estimated by using the \code{lm()} function.
Mahalanobis distance and hat values are computed for quantifying
\emph{leverage}, standardized leverage-corrected residuals and
studentized leverage-corrected residuals are computed for quantifying
\emph{distance}, and Cook`s distance and DfBetas are computed
for quantifying \emph{influence}.
}
\details{
In regression analysis, an observation can be extreme in three major ways (see
Darlington & Hayes, p. 484): (1) An observation has high \strong{leverage} if it
has a atypical pattern of values on the predictors, (2) an observation has high
\strong{distance} if its observed outcome value \eqn{Y_i} has a large deviation
from the predicted value \eqn{\hat{Y}_i}, and (3) an observation has high
\strong{influence} if its inclusion substantially changes the estimates for the
intercept and/or slopes.
}
\examples{
# Example 1: Regression model and measures for leverage, distance, and influence
mod.lm <- lm(mpg ~ cyl + disp + hp, data = mtcars)
check.outlier(mod.lm)

# Merge result table with the data
dat1 <- cbind(mtcars, check.outlier(mod.lm))
}
\references{
Darlington, R. B., &, Hayes, A. F. (2017). \emph{Regression analysis and linear
models}: Concepts, applications, and implementation. The Guilford Press.
}
\seealso{
\code{\link{check.collin}}, \code{\link{lm}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
