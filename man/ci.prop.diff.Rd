\name{ci.prop.diff}
\alias{ci.prop.diff}
\alias{ci.prop.diff.default}
\alias{ci.prop.diff.formula}
\title{Confidence Interval for the Difference in Proportions}
\usage{
ci.prop.diff(...)

\method{ci.prop.diff}{default}(data = NULL, ..., x = NULL, y = NULL,
             method = c("wald", "newcombe"), paired = FALSE,
             alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
             group = NULL, split = NULL, sort.var = FALSE, digits = 2, as.na = NULL,
             write = NULL, append = TRUE, check = TRUE, output = TRUE)

\method{ci.prop.diff}{formula}(formula, data, method = c("wald", "newcombe"),
             alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
             group = NULL, split = NULL, sort.var = FALSE, na.omit = FALSE,
             digits = 2, as.na = NULL, write = NULL, append = TRUE,
             check = TRUE, output = TRUE, \dots)
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

\item{method}{a character string specifying the method for computing the confidence interval,
must be one of \code{"wald"}, or \code{"newcombe"} (default).}

\item{paired}{logical: if \code{TRUE}, confidence interval for the difference of proportions
in paired samples is computed.}

\item{alternative}{a character string specifying the alternative hypothesis, must be one of
\code{"two.sided"} (default), \code{"greater"} or \code{"less"}.}

\item{na.omit}{logical: if \code{TRUE}, incomplete cases are removed before conducting the analysis
(i.e., listwise deletion) when specifying more than one outcome variable.}

\item{conf.level}{a numeric value between 0 and 1 indicating the confidence level of the interval.}

\item{group}{a numeric vector, character vector or factor as grouping variable. Note that a grouping
variable can only be used when computing confidence intervals with unknown population
standard deviation and population variance.}

\item{split}{a numeric vector, character vector or factor as split variable. Note that a split
variable can only be used when computing confidence intervals with unknown population
standard deviation and population variance.}

\item{sort.var}{logical: if \code{TRUE}, output table is sorted by variables when specifying \code{group}.}

\item{digits}{an integer value indicating the number of decimal places to be used.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before conducting the analysis.
Note that \code{as.na()} function is only applied to \code{x}, but
not to \code{group} or \code{split}.}

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
This function computes confidence intervals for the difference in proportions in a two-sample
and paired-sample design for one or more variables, optionally by a grouping and/or split variable.
}
\details{
The Wald confidence interval which is based on the normal approximation to the binomial distribution are
computed by specifying \code{method = "wald"}, while the Newcombe Hybrid Score interval (Newcombe, 1998a;
Newcombe, 1998b) is requested by specifying \code{method = "newcombe"}. By default, Newcombe Hybrid Score
interval is computed which have been shown to be reliable in small samples (less than n = 30 in each sample)
as well as moderate to larger samples(n > 30 in each sample) and with proportions close to 0 or 1, while the
Wald confidence intervals does not perform well unless the sample size is large (Fagerland, Lydersen & Laake, 2011).
}
\examples{
#————————————————————————————————————————————————————————————————————————————
# Two-Sample Design

# Example 1a: Two-Sided 95% CI for 'vs' by 'am', Newcombes Hybrid Score interval
ci.prop.diff(vs ~ am, data = mtcars)

# Example 2b: Alternative specification without using the '...' argument
ci.mean.diff(x = df.subset(mtcars, vs, subset = am == 0),
             y = df.subset(mtcars, vs, subset = am == 1))

# Example 1c: Wald CI
ci.prop.diff(vs ~ am, data = mtcars, method = "wald")

#————————————————————————————————————————————————————————————————————————————
# Paired-Sample Design

# Example 2a: Two-Sided 95% CI for the difference in proportions 'pre' and 'post'
# Newcombes Hybrid Score interval
ci.prop.diff(mtcars, vs, am, paired = TRUE)

# Example 2b:  Alternative specification without using the '...' argument
ci.prop.diff(x = mtcars$vs, y = mtcars$am, paired = TRUE)

# Example 2b: Wald CI
ci.prop.diff(mtcars, vs, am, method = "wald", paired = TRUE)
}
\references{
Fagerland, M. W., Lydersen S., & Laake, P. (2011) Recommended confidence intervals for two independent binomial
proportions. \emph{Statistical Methods in Medical Research, 24}, 224-254.

Newcombe, R. G. (1998a). Interval estimation for the difference between independent proportions: Comparison of
eleven methods. \emph{Statistics in Medicine, 17}, 873-890.

Newcombe, R. G. (1998b). Improved confidence intervals for the difference between binomial proportions based on
paired data. \emph{Statistics in Medicine, 17}, 2635-2650.

Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
John Wiley & Sons.
}
\seealso{
\code{\link{ci.prop}}, \code{\link{ci.mean}}, \code{\link{ci.mean.diff}},
\code{\link{ci.median}}, \code{\link{ci.var}}, \code{\link{ci.sd}},
\code{\link{descript}}
}
\author{
Takuya Yanagida
}
