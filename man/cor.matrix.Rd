% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cor.matrix.R
\name{cor.matrix}
\alias{cor.matrix}
\title{Correlation Matrix}
\usage{
cor.matrix(..., data = NULL,
           method = c("pearson", "spearman", "kendall-b", "kendall-c", "tetra", "poly"),
           na.omit = FALSE, group = NULL, sig = FALSE, alpha = 0.05,
           print = c("all", "cor", "n", "stat", "df", "p"),
           tri = c("both", "lower", "upper"),
           p.adj = c("none", "bonferroni", "holm", "hochberg", "hommel",
                     "BH", "BY", "fdr"), continuity = TRUE,
           digits = 2, p.digits = 3, as.na = NULL,
           write = NULL, append = TRUE, check = TRUE, output = TRUE)
}
\arguments{
\item{...}{a matrix or data frame. Alternatively, an expression indicating
the variable names in \code{data} e.g.,
\code{cor.matrix(x1, x2, x3, data = dat)}. Note that the operators
\code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
and \code{!} can also be used to select variables, see 'Details'
in the \code{\link{df.subset}} function.}

\item{data}{a data frame when specifying one or more variables in the
argument \code{...}. Note that the argument is \code{NULL}
when specifying a matrix or data frame for the argument \code{...}.}

\item{method}{a character vector indicating which correlation coefficient
is to be computed, i.e. \code{"pearson"} for Pearson product-moment correlation
coefficient (default), \code{"spearman"} for Spearman's rank-order correlation
coefficient, \code{"kendall-b"} for Kendall's Tau-b correlation coefficient,
\code{"kendall-c"} for Kendall-Stuart's Tau-c correlation coefficient,
\code{"tetra"} for tetrachoric correlation coefficient, and \code{"poly"} for
polychoric correlation coefficient.}

\item{na.omit}{logical: if \code{TRUE}, incomplete cases are removed before
conducting the analysis (i.e., listwise deletion); if \code{FALSE}
(default), pairwise deletion is used.}

\item{group}{either a character string indicating the variable name of
the grouping variable in \code{...} or \code{data}, or a
vector representing the grouping variable. Note that the
grouping variable is limited to two groups.}

\item{sig}{logical: if \code{TRUE}, statistically significant correlation
coefficients are shown in boldface on the console. Note that
this function does not provide statistical significance
testing for tetrachoric or polychoric correlation coefficients.}

\item{alpha}{a numeric value between 0 and 1 indicating the significance
level at which correlation coefficients are printed boldface
when \code{sig = TRUE}.}

\item{print}{a character string or character vector indicating which results
to show on the console, i.e. \code{"all"} for all results,
\code{"cor"} for correlation coefficients, \code{"n"} for the
sample sizes, \code{"stat"} for the test statistic, \code{"df"}
for the degrees of freedom, and \code{"p"} for \emph{p}-values.
Note that the function does not provide \emph{p}-values for
tetrachoric or polychoric correlation coefficients.}

\item{tri}{a character string indicating which triangular of the matrix
to show on the console, i.e., \code{both} for upper and lower
triangular, \code{lower} (default) for the lower triangular,
and \code{upper} for the upper triangular.}

\item{p.adj}{a character string indicating an adjustment method for multiple
testing based on \code{\link{p.adjust}}, i.e., \code{none} ,
\code{bonferroni}, \code{holm} (default), \code{hochberg},
\code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.}

\item{continuity}{logical: if \code{TRUE} (default), continuity correction is
used for testing Spearman's rank-order correlation coefficient
and Kendall's Tau-b correlation.}

\item{digits}{an integer value indicating the number of decimal places to be
used for displaying correlation coefficients.}

\item{p.digits}{an integer value indicating the number of decimal places to be
used for displaying \emph{p}-values.}

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
  \item{\code{result}}{list with result tables, i.e., \code{cor} for the
                       correlation matrix, \code{n} for a matrix with the sample
                       sizes, \code{stat} for a matrix with the test statistics,
                       \code{df} for a matrix with the degrees of freedom, and
                       \code{p}-value for the matrix with the significance values
                       (\emph{p}-values)}
}
\description{
This function computes a correlation matrix based on Pearson product-moment
correlation coefficient, Spearman's rank-order correlation coefficient,
Kendall's Tau-b correlation coefficient, Kendall-Stuart's Tau-c correlation
coefficient, tetrachoric correlation coefficient, or polychoric correlation
coefficient and computes significance values (\emph{p}-values) for testing the
hypothesis H0: \eqn{\rho} = 0 for all pairs of variables.
}
\details{
Note that unlike the \code{\link[stats:cor.test]{cor.test}} function, this
function does not compute an exact \emph{p}-value for Spearman's rank-order
correlation coefficient or Kendall's Tau-b correlation coefficient, but uses
the asymptotic \emph{t} approximation.

Statistically significant correlation coefficients can be shown in boldface on
the console when specifying \code{sig = TRUE}. However, this option is not supported
when using R Markdown, i.e., the argument \code{sig} will switch to \code{FALSE}.
}
\note{
This function uses the \code{polychoric()} function in the \pkg{psych}
package by William Revelle to estimate tetrachoric and polychoric correlation
coefficients.
}
\examples{
# Example 1a: Pearson product-moment correlation coefficient between 'Ozone' and 'Solar.R'
cor.matrix(airquality[, c("Ozone", "Solar.R")])

# Example 1b: Alternative specification using the 'data' argument
cor.matrix(Ozone, Solar.R, data = airquality)

# Example 2a: Pearson product-moment correlation matrix using pairwise deletion
cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")])

# Example 2b: Alternative specification using the 'data' argument
cor.matrix(Ozone:Wind, data = airquality)

# Example 3: Spearman's rank-order correlation matrix
cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")], method = "spearman")

# Example 4: Pearson product-moment correlation matrix
# highlight statistically significant result at alpha = 0.05
cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")], sig = TRUE)

# Example 5: Pearson product-moment correlation matrix
# highlight statistically significant result at alpha = 0.05
cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")], sig = TRUE, alpha = 0.10)

# Example 6: Pearson product-moment correlation matrix
# print sample size and significance values
cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")], print = "all")

# Example 7: Pearson product-moment correlation matrix using listwise deletion,
# print sample size and significance values
cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")], na.omit = TRUE, print = "all")

# Example 8: Pearson product-moment correlation matrix
# print sample size and significance values with Bonferroni correction
cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")], na.omit = TRUE,
           print = "all", p.adj = "bonferroni")

# Example 9a: Pearson product-moment correlation matrix for 'mpg', 'cyl', and 'disp'
# results for group "0" and "1" separately
cor.matrix(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs)

# Example 9b: Alternative specification using the 'data' argument
cor.matrix(mpg:disp, data = mtcars, group = "vs")

\dontrun{
# Example 10a: Write results into a text file
cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")], print = "all", write = "Correlation.txt")

# Example 10b: Write results into an Excel file
cor.matrix(airquality[, c("Ozone", "Solar.R", "Wind")], print = "all", write = "Correlation.xlsx")
}
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
- Using R and SPSS}. John Wiley & Sons.

Revelle, W. (2018) \emph{psych: Procedures for personality and psychological
research}. Northwestern University, Evanston, Illinois, USA,
https://CRAN.R-project.org/package=psych Version = 1.8.12.
}
\seealso{
\code{\link{write.result}}, \code{\link{cohens.d}}, \code{\link{effsize}},
\code{\link{multilevel.icc}}, \code{\link{na.auxiliary}}, \code{\link{size.cor}}.
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
