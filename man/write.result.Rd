\name{write.result}
\alias{write.result}
\title{Write Results of a misty Object into an Excel file}
\usage{
write.result(x, file = "Results.xlsx", digits = NULL, p.digits = NULL,
             icc.digits = NULL, r.digits = NULL, ess.digits = NULL,
             mcse.digits = NULL, check = TRUE)
}
\arguments{
\item{x}{misty object (\code{misty.object}) resulting from a misty function
supported by the \code{write.result} function (see 'Details').}

\item{file}{a character string naming a file with or without file extension
'.xlsx', e.g., \code{"Results.xlsx"} or \code{"Results"}.}

\item{digits}{an integer value indicating the number of decimal places digits
to be used for displaying results.}

\item{p.digits}{an integer indicating the number of decimal places to be used
for displaying \emph{p}-values.}

\item{icc.digits}{an integer indicating the number of decimal places to be used
for displaying intraclass correlation coefficients.}

\item{r.digits}{an integer value indicating the number of decimal places
to be used for displaying R-hat values, item-total correlation
standardized factor loadings, coefficient alpha, and
coefficient omega.}

\item{ess.digits}{an integer value indicating the number of decimal places
to be used for displaying effective sample sizes.}

\item{mcse.digits}{an integer value indicating the number of decimal places
to be used for displaying Monte Carlo standard errors.}

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}
}
\description{
This function writes the results of a \code{misty.object}) into an Excel file.
}
\author{
Takuya Yanagida
}
