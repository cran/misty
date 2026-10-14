\name{print.misty.object}
\alias{print.misty.object}
\title{Print misty.object object}
\usage{
\method{print}{misty.object}(x, digits = NULL, p.digits = NULL, icc.digits = NULL,
      r.digits = NULL, ess.digits = NULL, mcse.digits = NULL, horiz = TRUE,
      check = TRUE, ...)
}
\arguments{
\item{x}{\code{misty.object} object.}

\item{digits}{an integer value indicating the number of decimal places
to be used for displaying results.}

\item{p.digits}{an integer indicating the number of decimal places to be used
for displaying \emph{p}-values.}

\item{icc.digits}{an integer indicating the number of decimal places to be used
for displaying intraclass correlation coefficients
(\code{multilevel.descript()} and \code{multilevel.icc()}
function).}

\item{r.digits}{an integer value indicating the number of decimal places
to be used for displaying R-hat values, item-total correlation
standardized factor loadings, coefficient alpha, and
coefficient omega.}

\item{ess.digits}{an integer value indicating the number of decimal places
to be used for displaying effective sample sizes.}

\item{mcse.digits}{an integer value indicating the number of decimal places
to be used for displaying Monte Carlo standard errors.}

\item{horiz}{logical: if \code{TRUE}, a horizontal line under the table
header is drawn.}

\item{check}{logical: if \code{TRUE}, argument specification is checked.}

\item{...}{further arguments passed to or from other methods.}
}
\description{
This function prints an \code{misty.object} object.
}
\author{
Takuya Yanagida
}
