\name{test.bm}
\alias{test.bm}
\alias{test.bm.default}
\alias{test.bm.formula}
\title{Brunner-Munzel Test for Stochastic Equality}
\usage{
test.bm(...)

\method{test.bm}{default}(data = NULL, ..., x = NULL, y = NULL, paired = FALSE,
        method = c("t", "logit", "perm"), alternative = c("two.sided", "less", "greater"),
        scale = c("prob", "diff", "logodds", "odds"), hypo = FALSE, descript = TRUE,
        conf.level = 0.95, nperm = 10000, seed = 42, digits = 3, p.digits = 3,
        as.na = NULL, write = NULL, append = TRUE, check = TRUE, output = TRUE)

\method{test.bm}{formula}(formula, data, method = c("t", "logit", "perm"),
        alternative = c("two.sided", "less", "greater"),
        scale = c("prob", "diff", "logodds", "odds"), hypo = FALSE, descript = TRUE,
        conf.level = 0.95, p.adj = "none", nperm = 10000, seed = 42, na.omit = FALSE,
        digits = 3, p.digits = 3, as.na = NULL, write = NULL, append = TRUE,
        check = TRUE, output = TRUE, ...)
}
\arguments{
\item{data}{a data frame. Note that data are specified either by using
the argument \code{data} or by using the arguments \code{x}
and \code{y}.}

\item{...}{an expression indicating the variable names in \code{data},
e.g., \code{test.bm(dat, x1, x2)}. Note that the operators
\code{+}, \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
can also be used to select variables, see 'Details' in the
\code{\link{df.subset}} function.}

\item{x}{a numeric vector of data values.}

\item{y}{a numeric vector of data values.}

\item{formula}{in case of two sample Brunner-Munzel test (i.e., \code{paired = FALSE}),
a formula of the form \code{y ~ group} where \code{group}
is a numeric variable, character variable or factor with
two values or factor levels giving the corresponding
groups.}

\item{paired}{logical: if \code{TRUE}, paired-sample Brunner-Munzel-test
is computed.}

\item{method}{a character string indicating the method used for statistical
significance testing, i.e., \code{"t"} for a t-distribution
with Satterthwaite-Welch degrees of freedom approximation,
\code{"logit"} for a t-distribution with Satterthwaite-Welch
degrees of freedom approximation and logit transformation
for range-preserving confidence intervals, and \code{"perm"}
for a studentized permutation or randomization test that
is recommended for a better control of the type I error
rate when sample sizes are small (smaller than 15 per group).}

\item{alternative}{a character string specifying the alternative hypothesis,
must be one of \code{"two.sided"} (default),
\code{"greater"} or \code{"less"}.}

\item{scale}{a character string indicating the scale of the reported
estimated and confidence interval, i.e., \code{"prob"}
(default) for \eqn{P(X < Y) + 0.5(P(X = Y)} with range
\eqn{[0, 1]} and a null value of \eqn{0.5}, \code{"diff"}
for \eqn{P(X < Y) - P(X > Y)}  with range \eqn{[-1, 1]}
and a null value of \eqn{0}, \code{"logodds"} for
\eqn{log[P/(1 - P)]} with range \eqn{[-Inf, Inf]} and a
null value of \eqn{0}, and \code{"odds"} for \eqn{P/(1 - P)}
with range \eqn{[0, Inf]} and a null value of \eqn{1}.
Note that the statistical test always operates on the probability
scale.}

\item{hypo}{logical: if \code{TRUE} (default), null and alternative
hypothesis are shown on the console.}

\item{descript}{logical: if \code{TRUE} (default), descriptive statistics
are shown on the console.}

\item{conf.level}{a numeric value between 0 and 1 indicating the confidence
level of the interval.}

\item{p.adj}{a character string indicating an adjustment method for multiple
testing based on \code{\link{p.adjust}}, i.e., \code{none} (default),
\code{bonferroni}, \code{holm}, \code{hochberg},
\code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.
See help page of the \code{\link{p.adjust}} function.}

\item{nperm}{a numeric value (default: 10000) indicating the maximum
number of permutations used when \code{method = "perm"}.
If the number of complete permutations, i.e., \code{choose(N, n.x)}
in the two-sample design and \code{2^n} in the paired-sample
design, is smaller than or equal \code{nperm}, an exact
permutation test is conducted, while a randomization test
based on \code{nperm} samples of unique permutations is
conducted when the number of complete permutations is
greater than \code{nperm}.}

\item{seed}{a numeric value specifying the seed of the pseudo-random
numbers used when drawing permutation samples.}

\item{na.omit}{logical: if \code{TRUE}, incomplete cases are removed
before conducting the analysis (i.e., listwise deletion)
when specifying more than one outcome variable.}

\item{digits}{an integer value indicating the number of decimal .places
to be used for displaying descriptive statistics and
confidence interval.}

\item{p.digits}{an integer value indicating the number of decimal .places
to be used for displaying the \emph{p}-value.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before
conducting the analysis.}

\item{write}{a character string naming a text file with file extension
\code{".txt"} (e.g., \code{"Output.txt"}) for writing the
output into a text file.}

\item{append}{logical: if \code{TRUE} (default), output will be appended
to an existing text file with extension \code{.txt} specified
in \code{write}, if \code{FALSE} existing text file will be
overwritten.}

\item{check}{logical: if \code{TRUE} (default), argument specification
is checked.}

\item{output}{logical: if \code{TRUE} (default), output is shown on the
console.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:
\item{\code{call}}{function call}
\item{\code{type}}{type of analysis}
\item{\code{sample}}{type of sample, i.e., two- or paired sample}
\item{\code{formula}}{formula}
\item{\code{data}}{data frame with the outcome and grouping variable}
\item{\code{args}}{specification of function arguments}
\item{\code{result}}{result table}
}
\description{
This function performs two-sample and paired-sample (Permuted) Brunner-Munzel
test (Brunner & Munzel, 2000) using either a t-distribution with a Satterthwaite-Welch degrees of freedom
approximation, an exact permutation test, or a randomization test (Neubert & Brunner, 2007).
}
\note{
This function is based on the \code{brunner_munzel()} and \code{ses_calc()}
function in the \pkg{TOSTER} package by Daniel Lakens and Aaron Caldwell.
}
\examples{
#————————————————————————————————————————————————————————————————————————————
# Two-Sample Design

# Example 1a: Two-sided two-sample Brunner-Munzel test
test.bm(mpg ~ vs, data = mtcars)

# Example 1b: Alternative specification without using the '...' argument
test.bm(x = df.subset(mtcars, mpg, subset = vs == 0),
        y = df.subset(mtcars, mpg, subset = vs == 1))

# Example 1c: Two-sided two-sample Brunner-Munzel randomization test
# Sample of 10000 out of choose(32, 18) = 471435600 permutations
test.bm(mpg ~ vs, data = mtcars, method = "perm")

# Example 1d: Two-sided two-sample Brunner-Munzel permutation test
# Complete permutation of choose(13, 8) = 1287 permutations
test.bm(x = c(3, 1, 4, 2, 5, 3, 6, 7), y = c(5, 2, 4, 3, 1), method = "perm")

# Example 1e: One-sided two-sample Brunner-Munzel test
test.bm(mpg ~ vs, data = mtcars, alternative = "greater")

# Example 1f: Multiple two-sample Brunner-Munzel tests
test.bm(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars)

# Example 1g: Multiple two-sample Brunner-Munzel tests, bonferroni correction
test.bm(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars, p.adj = "bonferroni")

#————————————————————————————————————————————————————————————————————————————
# Paired-Sample Design

# Example 2a: Two-sided paired-sample Brunner-Munzel test
test.bm(mtcars, drat, wt, paired = TRUE)

# Example 2b: Alternative specification without using the '...' argument
test.bm(mtcars[, c("drat", "wt")], paired = TRUE)

# Example 2c: Alternative specification without using the '...' argument
test.bm(x = mtcars$drat, y = mtcars$wt, paired = TRUE)

# Example 2d: Two-sided paired-sample Brunner-Munzel test with logit transformation
test.bm(mtcars, drat, wt, paired = TRUE, method = "logit")

\dontrun{
#————————————————————————————————————————————————————————————————————————————
# Write Results

# Example 3a: Write results into a text file
test.bm(mpg ~ vs, data = mtcars, write = "BM-Test.txt")

# Example 3b:  Write results into an Excel file
test.bm(mpg ~ vs, data = mtcars, write = "BM-Test.xlsx")
}
}
\references{
Schüürhuis, S., Konietschke, F., & Brunner, E. (2025). A New Approach to the
nonparametric Behrens-Fisher problem With compatible confidence iIntervals.
\emph{Biometrical Journal, 67}(6), e70096. https://doi.org/10.1002/bimj.70096

Karch, J. D. (2021). Psychologists should use Brunner-Munzel’s instead of
Mann-Whitney’s U Test as the default nonparametric procedure.
\emph{Advances in Methods and Practices in Psychological Science, 4}(2).
https://doi.org/10.1177/2515245921999

Neubert, K., & Brunner, E. (2007). A studentized permutation test for the
non-parametric Behrens-Fisher problem. \emph{Computational Statistics & Data Analysis, 51}(10),
5192–5204. https://doi.org/10.1016/j.csda.2006.05.024
}
\seealso{
\code{\link{test.t}}, \code{\link{test.welch}}, \code{\link{test.z}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
