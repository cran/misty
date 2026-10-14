\name{test.t}
\alias{test.t}
\alias{test.t.default}
\alias{test.t.formula}
\title{t-Test}
\usage{
test.t(...)

\method{test.t}{default}(data = NULL, ..., x = NULL, y = NULL, mu = 0,
       paired = FALSE, alternative = c("two.sided", "less", "greater"),
       hypo = FALSE, descript = TRUE, effsize = FALSE, weighted = TRUE,
       correct = FALSE, conf.level = 0.95, digits = 2, p.digits = 3, as.na = NULL,
       plot = FALSE, geom = c("all", "bar", "point", "ci", "line", "jitter"),
       adjust = TRUE, filename = NULL, width = NA, height = NA, dpi = 600,
       write = NULL, append = TRUE, check = TRUE, output = TRUE)

\method{test.t}{formula}(formula, data, alternative = c("two.sided", "less", "greater"),
       hypo = FALSE, descript = TRUE, effsize = FALSE, weighted = TRUE, correct = FALSE,
       conf.level = 0.95, p.adj = "none", na.omit = FALSE, digits = 2, p.digits = 3,
       as.na = NULL, plot = FALSE, geom = c("all", "bar", "point", "ci", "jitter"),
       adjust = TRUE, filename = NULL, width = NA, height = NA, dpi = 600,
       write = NULL, append = TRUE, check = TRUE, output = TRUE, ...)
}
\arguments{
\item{data}{a data frame. Note that data are specified either by using
the argument \code{data} or by using the arguments \code{x}
and \code{y}.}

\item{...}{an expression indicating the variable names in \code{data},
e.g., \code{test.t(dat, x1, x2)}. Note that the operators
\code{+}, \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
can also be used to select variables, see 'Details' in the
\code{\link{df.subset}} function.}

\item{x}{a numeric vector of data values.}

\item{y}{a numeric vector of data values.}

\item{formula}{in case of two sample t-test (i.e., \code{paired = FALSE}),
a formula of the form \code{y ~ group} where \code{group}
is a numeric variable, character variable or factor with
two values or factor levels giving the corresponding
groups.}

\item{mu}{a numeric value indicating the population mean under the
null hypothesis. Note that the argument \code{mu} is only
used when computing a one sample t-test.}

\item{paired}{logical: if \code{TRUE}, paired-sample t-test is computed.}

\item{alternative}{a character string specifying the alternative hypothesis,
must be one of \code{"two.sided"} (default),
\code{"greater"} or \code{"less"}.}

\item{hypo}{logical: if \code{TRUE} (default), null and alternative
hypothesis are shown on the console.}

\item{descript}{logical: if \code{TRUE} (default), descriptive statistics
are shown on the console.}

\item{effsize}{logical: if \code{TRUE}, effect size measure Cohen's d is
shown on the console, see \code{\link{cohens.d}} function.}

\item{weighted}{logical: if \code{TRUE} (default), the weighted pooled standard
deviation is used to compute Cohen's d for a two-sample design (i.e., \code{paired = FALSE}),
while standard deviation of the difference scores is used to compute Cohen's d
for a paired-sample design (i.e., \code{paired = TRUE}).}

\item{correct}{logical: if \code{TRUE}, correction factor to remove
positive bias in small samples is used  when computing Cohen's d.}

\item{conf.level}{a numeric value between 0 and 1 indicating the confidence
level of the interval.}

\item{p.adj}{a character string indicating an adjustment method for multiple
testing based on \code{\link{p.adjust}}, i.e., \code{none} (default),
\code{bonferroni}, \code{holm}, \code{hochberg},
\code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.
See help page of the \code{\link{p.adjust}} function.}

\item{na.omit}{logical: if \code{TRUE}, incomplete cases are removed
before conducting the analysis (i.e., listwise deletion)
when specifying more than one outcome variable.}

\item{digits}{an integer value indicating the number of decimal places
to be used for displaying descriptive statistics and
confidence interval.}

\item{p.digits}{an integer value indicating the number of decimal places
to be used for displaying the \emph{p}-value.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before
conducting the analysis.}

\item{plot}{logical: if \code{TRUE}, a plot showing bar plots with
error bars for confidence intervals is drawn. For additional plotting arguments,
see \code{Details} in the help page of the function \code{plot.misty.object}.}

\item{geom}{a character string or character vector indicating which
plot elements to draw, i.e., \code{"all"} for drawing all plot elements,
\code{"bar"} for drawing bars representing means for each
groups, \code{"point"} for drawing points representing
means for each group, \code{"ci"} for drawing error bars
representing confidence intervals, \code{"jitter"} for
drawing jittered data points, \code{"line"} for drawing
a horizontal line at \code{mu} for the one-sample t-test
or at \code{0} for the paired-sample t-test.}

\item{adjust}{logical: if \code{TRUE} (default), difference-adjustment
for the confidence intervals in a two-sample design is
applied.}

\item{filename}{a character string indicating the \code{filename}
argument including the file extension in the \code{ggsave}
function. Note that one of \code{".eps"}, \code{".ps"},
\code{".tex"}, \code{".pdf"} (default),
\code{".jpeg"}, \code{".tiff"}, \code{".png"},
\code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
to be specified as file extension in the \code{file}
argument. Note that plots can only be saved when
\code{plot = TRUE}.}

\item{width}{a numeric value indicating the \code{width} argument
(default is the size of the current graphics device)
for the \code{ggsave} function.}

\item{height}{a numeric value indicating the \code{height} argument
(default is the size of the current graphics device)
for the \code{ggsave} function.}

\item{dpi}{a numeric value indicating the \code{dpi} argument
(default is \code{600}) for the \code{ggsave} function.}

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
\item{\code{sample}}{type of sample, i.e., one-, two-, or paired sample}
\item{\code{formula}}{formula}
\item{\code{data}}{data frame with the outcome and grouping variable}
\item{\code{args}}{specification of function arguments}
\item{\code{plot}}{ggplot2 object for plotting the results}
\item{\code{result}}{result table}
}
\description{
This function performs one-sample, two-sample, and paired-sample t-tests and
provides descriptive statistics, effect size measure, and a plot showing bar
plots with error bars for (difference-adjusted) confidence intervals.
}
\examples{
#————————————————————————————————————————————————————————————————————————————
# One-Sample Design

# Example 1a: Two-sided one-sample t-test, population mean = 20
test.t(mtcars, mpg, mu = 20)

# Example 1b: Alternative specification without using the '...' argument
test.t(mtcars$mpg, mu = 20)

# Example 1c: One-sided one-sample t-test, population mean = 20, print Cohen's d
test.t(mtcars, mpg, mu = 20, alternative = "greater", effsize = TRUE)

#————————————————————————————————————————————————————————————————————————————
# Two-Sample Design

# Example 2a: Two-sided two-sample t-test
test.t(mpg ~ vs, data = mtcars)

# Example 2b: Alternative specification without using the '...' argument
test.t(x = df.subset(mtcars, mpg, subset = vs == 0),
       y = df.subset(mtcars, mpg, subset = vs == 1))

# Example 2c: One-sided two-sample t-test, print Cohen's d
test.t(mpg ~ vs, data = mtcars, alternative = "greater", effsize = TRUE)

# Example 2d: Multiple two-sample t-tests
test.t(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars)

# Example 2e: Multiple two-sample t-tests, bonferroni correction
test.t(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars, p.adj = "bonferroni")

#————————————————————————————————————————————————————————————————————————————
# Paired-Sample Design

# Example 3a: Two-sided paired-sample t-test
test.t(mtcars, drat, wt, paired = TRUE)

# Example 3b:  Alternative specification without using the '...' argument
test.t(mtcars[, c("drat", "wt")], paired = TRUE)

# Example 3c: Alternative specification without using the '...' argument
test.t(x = mtcars$drat, y = mtcars$wt, paired = TRUE)

# Example 3d: One-sided paired-sample t-test, print Cohen's d
test.t(mtcars, drat, wt, paired = TRUE, alternative = "greater", effsize = TRUE)

#————————————————————————————————————————————————————————————————————————————
# Plot

# Example 4a: One-Sample Design
test.t(mtcars, mpg, mu = 20, plot = TRUE)

# Example 4b: Two-Sample Design
test.t(mpg ~ vs, data = mtcars, plot = TRUE)

# Example 4c: Paired-Sample Design
test.t(mtcars, drat, wt, paired = TRUE, plot = TRUE)

# Example 4d: Plot results using the plot() function, use additional arguments
# see Details in the help page of the function plot.misty.object
object <- test.t(mpg ~ vs, data = mtcars)
plot(object, geom = c("bar", "ci", "jitter"), jitter.alpha = 0.4, title = "Two-Sample t-Test")

#————————————————————————————————————————————————————————————————————————————
# Create Plot Manually

# Load ggplot2 package
library(ggplot2)

# Example 4a: Two-sample t-test
ci.table <- ci.mean(mtcars, mpg, group = "vs", adjust = TRUE, output = FALSE)$result

ggplot(ci.table, aes(group, m)) +
  geom_bar(aes(group, m), stat = "summary", fun = "mean") +
  geom_errorbar(aes(group, m, ymin = low, ymax = upp), width = 0.1) +
  theme_bw()

# Example 4b: Paired-sample t-test
object <- test.t(mtcars, drat, wt, paired = TRUE)

ggplot(data.frame(x = object$data$y - object$data$x), aes(x = 0L, y = x)) +
 geom_bar(data = object$result, aes(0, m.diff), stat = "summary", fun = "mean") +
 geom_errorbar(data = object$result, aes(0, m.diff, ymin = m.low, ymax = m.upp), width = 0.1) +
 geom_hline(yintercept = 0L, linetype = 3, linewidth = 0.8) +
 scale_x_continuous(name = "", limits = c(-2, 2)) +
 theme_bw()  +
 theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

\dontrun{
#————————————————————————————————————————————————————————————————————————————
# Write Results and Save Plot

# Example 6a: Write results into a text file
test.t(mpg ~ vs, data = mtcars, write = "t-Test.txt")

# Example 6b:  Write results into an Excel file
test.t(mpg ~ vs, data = mtcars, write = "t-Test.xlsx")

# Example 6c: Save plot as PNG fine
test.t(mpg ~ vs, data = mtcars, plot = TRUE, filename = "t-Test.png",
       width = 6, height = 5)
}
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in
psychology - Using R and SPSS}. John Wiley & Sons.
}
\seealso{
\code{\link{aov.b}}, \code{\link{aov.w}}, \code{\link{test.welch}},
\code{\link{test.z}}, \code{\link{test.levene}}, \code{\link{cohens.d}},
\code{\link{ci.mean.diff}}, \code{\link{ci.mean}}
}
\author{
Takuya Yanagida
}
