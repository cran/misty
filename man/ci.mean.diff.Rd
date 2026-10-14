\name{ci.mean.diff}
\alias{ci.mean.diff}
\alias{ci.mean.diff.default}
\alias{ci.mean.diff.formula}
\title{Confidence Interval for the Difference in Arithmetic Means}
\usage{
ci.mean.diff(...)

\method{ci.mean.diff}{default}(data = NULL, ..., x = NULL, y, mu = 0,
             sigma = NULL, sigma2 = NULL, var.equal = FALSE, paired = FALSE,
             alternative = c("two.sided", "less", "greater"),
             conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
             digits = 2, as.na = NULL, write = NULL, append = TRUE,
             check = TRUE, output = TRUE)

\method{ci.mean.diff}{formula}(formula, data, sigma = NULL, sigma2 = NULL,
             var.equal = FALSE, alternative = c("two.sided", "less", "greater"),
             conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
             na.omit = FALSE, digits = 2, as.na = NULL, write = NULL,
             append = TRUE, check = TRUE, output = TRUE, ...)
}
\arguments{
\item{data}{a data frame. Note that data are specified either by using
the argument \code{data} or by using the arguments \code{x}
and \code{y}.}

\item{...}{an expression indicating the variable names in \code{data},
e.g., \code{ci.mean.diff(dat, x1, x2)}. Note that the operators
\code{+}, \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
can also be used to select variables, see 'Details' in the
\code{\link{df.subset}} function.}

\item{x}{a numeric vector of data values.}

\item{y}{a numeric vector of data values.}

\item{formula}{a formula of the form \code{y ~ group} for one outcome variable
or \code{cbind(y1, y2, y3) ~ group} for more than one outcome
variable where \code{y} is a numeric variable giving the data
values and \code{group} a numeric variable, character variable
or factor with two values or factor levels giving the corresponding
groups.}

\item{mu}{a numeric value indicating the population mean under the
null hypothesis. Note that the argument \code{mu} is only
used when \code{y = NULL}.}

\item{sigma}{a numeric vector indicating the population standard deviation(s)
when computing confidence intervals for the difference in
arithmetic means with known standard deviation(s). In case
of independent samples, equal standard deviations are assumed
when specifying one value for the argument \code{sigma}; when
specifying two values for the argument \code{sigma}, unequal
standard deviations are assumed. Note that either argument
\code{sigma} or argument \code{sigma2} is specified and it
is only possible to specify one value (i.e., equal variance
assumption) or two values (i.e., unequal variance assumption)
for the argument \code{sigma} even though multiple variables
are specified in \code{x}.}

\item{sigma2}{a numeric vector indicating the population variance(s) when
computing confidence intervals for the difference in arithmetic
means with known variance(s). In case of independent samples,
equal variances are assumed when specifying one value for the
argument \code{sigma2}; when specifying two values for the
argument \code{sigma}, unequal variances are assumed. Note
that either argument \code{sigma} or argument \code{sigma2}
is specified and it is only possible to specify one value
(i.e., equal variance assumption) or two values (i.e., unequal
variance assumption) for the argument \code{sigma} even though
multiple variables are specified in \code{x}.}

\item{var.equal}{logical: if \code{TRUE}, the population variance in the
independent samples are assumed to be equal.}

\item{paired}{logical: if \code{TRUE}, confidence interval for the difference
of arithmetic means in paired samples is computed.}

\item{alternative}{a character string specifying the alternative hypothesis,
must be one of \code{"two.sided"} (default), \code{"greater"}
or \code{"less"}.}

\item{na.omit}{logical: if \code{TRUE}, incomplete cases are removed before
conducting the analysis (i.e., listwise deletion) when specifying
more than one outcome variable.}

\item{conf.level}{a numeric value between 0 and 1 indicating the confidence
level of the interval.}

\item{group}{a numeric vector, character vector or factor as grouping
variable. Note that a grouping variable can only be used
when computing confidence intervals with unknown population
standard deviation and population variance.}

\item{split}{a numeric vector, character vector or factor as split variable.
Note that a split variable can only be used when computing
confidence intervals with unknown population}

\item{sort.var}{logical: if \code{TRUE}, output table is sorted by variables
when specifying \code{group}.}

\item{digits}{an integer value indicating the number of decimal places to
be used.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before conducting
the analysis. Note that \code{as.na()} function is only applied
to \code{x}, but not to \code{group} or \code{split}.}

\item{write}{a character string naming a text file with file extension
\code{".txt"} (e.g., \code{"Output.txt"}) for writing the
output into a text file.}

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
  \item{\code{data}}{list with the input specified in \code{x}, \code{group},
                     and \code{split}}
  \item{\code{args}}{specification of function arguments}
  \item{\code{result}}{result table}
}
\description{
This function computes confidence intervals for the difference in arithmetic
means in a one-sample, two-sample and paired-sample design with known or unknown
population standard deviation or population variance for one or more variables,
optionally by a grouping and/or split variable.
}
\examples{
#————————————————————————————————————————————————————————————————————————————
# One-Sample Design

# Example 1a: Two-Sided 95% CI for 'mpg', population mean = 20
ci.mean.diff(mtcars, mpg, mu = 20)

# Example 1b: Alternative specification without using the '...' argument
ci.mean.diff(mtcars$mpg, mu = 20)

#————————————————————————————————————————————————————————————————————————————
# Two-Sample Design

# Example 2a: Two-Sided 95% CI for 'mpg' by 'vs'
ci.mean.diff(mpg ~ vs, data = mtcars)

# Example 2b: Alternative specification without using the '...' argument
ci.mean.diff(x = df.subset(mtcars, mpg, subset = vs == 0),
             y = df.subset(mtcars, mpg, subset = vs == 1))

# Example 2c: Equal variance assumption
ci.mean.diff(mpg ~ vs, data = mtcars, var.equal = TRUE)

# Example 2d: Known equal population standard deviations
ci.mean.diff(mpg ~ vs, data = mtcars, sigma = 4)

# Example 2e: Known unequal population standard deviations
ci.mean.diff(mpg ~ vs, data = mtcars, sigma = c(4, 5))

# Example 2f: Two-Sided 95% CI for 'mpg', 'cyl', and 'disp' by 'vs'
ci.mean.diff(cbind(mpg, cyl, disp) ~ vs, data = mtcars)

# Example 2g: Analysis by group separately
ci.mean.diff(cbind(mpg, cyl, disp) ~ vs, data = mtcars, group = "am")

# Example 2h: Analysis split by group
ci.mean.diff(cbind(mpg, cyl, disp) ~ vs, data = mtcars, split = "am")

#————————————————————————————————————————————————————————————————————————————
# Paired-Sample Design

# Example 3a: Two-Sided 95% CI for the mean difference in 'pre' and 'post'
ci.mean.diff(mtcars, drat, wt, paired = TRUE)

# Example 3a: Alternative specification without using the '...' argument
ci.mean.diff(x = mtcars$drat, y = mtcars$wt, paired = TRUE)

# Example 3b: Analysis by group separately
ci.mean.diff(mtcars, drat, wt, paired = TRUE, group = "vs")

# Example 3c: Analysis split by group
ci.mean.diff(mtcars, drat, wt, paired = TRUE, split = "vs")

# Example 3e: Known population standard deviation of difference scores
ci.mean.diff(mtcars, drat, wt, sigma = 1, paired = TRUE)
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
- Using R and SPSS}. John Wiley & Sons.
}
\seealso{
\code{\link{test.z}}, \code{\link{test.t}}, \code{\link{ci.mean}}, \code{\link{ci.median}},
\code{\link{ci.prop}}, \code{\link{ci.var}}, \code{\link{ci.sd}}, \code{\link{descript}}
}
\author{
Takuya Yanagida
}
