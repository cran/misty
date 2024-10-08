% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/na.coverage.R
\name{na.coverage}
\alias{na.coverage}
\title{Variance-Covariance Coverage}
\usage{
na.coverage(..., data = NULL, tri = c("both", "lower", "upper"), digits = 2,
            as.na = NULL, write = NULL, append = TRUE, check = TRUE,
            output = TRUE)
}
\arguments{
\item{...}{a matrix or data frame with incomplete data, where missing
values are coded as \code{NA}. Alternatively, an expression
indicating the variable names in \code{data} e.g.,
\code{na.coverage(x1, x2, x3, data = dat)}. Note that the operators
\code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
and \code{!} can also be used to select variables, see 'Details'
in the \code{\link{df.subset}} function.}

\item{data}{a data frame when specifying one or more variables in the
argument \code{...}. Note that the argument is \code{NULL}
when specifying a matrix or data frame for the argument \code{...}.}

\item{tri}{a character string or character vector indicating which triangular
of the matrix to show on the console, i.e., \code{both} for
upper and lower triangular, \code{lower} (default) for the
lower triangular, and \code{upper} for the upper triangular.}

\item{digits}{an integer value indicating the number of decimal places to
be used for displaying proportions.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before conducting
the analysis.}

\item{write}{a character string naming a file for writing the output into
either a text file with file extension \code{".txt"} (e.g.,
\code{"Output.txt"}) or Excel file with file extension
\code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
name does not contain any file extension, an Excel file will
be written.}

\item{append}{logical: if \code{TRUE} (default), output will be appended
to an existing text file with extension \code{.txt} specified
in \code{write}, if \code{FALSE} existing text file will be
overwritten.}

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}

\item{output}{logical: if \code{TRUE} (default), output is shown on the console.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:
  \item{\code{call}}{function call}
  \item{\code{type}}{type of analysis}
  \item{\code{data}}{data frame used for the current analysis}
  \item{\code{args}}{specification of function arguments}
  \item{\code{result}}{result table}
}
\description{
This function computes the proportion of cases that contributes for the calculation
of each variance and covariance.
}
\examples{
# Example 1a: Compute variance-covariance coverage
na.coverage(airquality)

# Example 1b: Alternative specification using the 'data' argument
na.coverage(., data = airquality)

\dontrun{
# Example 2a: Write Results into a text file
na.coverage(airquality, write = "Coverage.txt")

# Example 2b: Write Results into an Excel file
na.coverage(airquality, write = "Coverage.xlsx")

result <- na.coverage(airquality, output = FALSE)
write.result(result, "Coverage.xlsx")
}
}
\references{
Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.

Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
\emph{Annual Review of Psychology, 60}, 549-576. https://doi.org/10.1146/annurev.psych.58.110405.085530

van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.).
Chapman & Hall.
}
\seealso{
\code{\link{write.result}}, \code{\link{as.na}}, \code{\link{na.as}},
\code{\link{na.auxiliary}}, \code{\link{na.descript}}, \code{\link{na.indicator}},
\code{\link{na.pattern}}, \code{\link{na.prop}}, \code{\link{na.test}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
