\name{test.welch}
\alias{test.welch}
\title{Welch's Test}
\usage{
test.welch(formula, data, alternative = c("two.sided", "less", "greater"),
           hypo = FALSE, descript = c(FALSE, TRUE), effsize = FALSE,
           weighted = FALSE, correct = FALSE, posthoc = FALSE, conf.level = 0.95,
           p.adj = "none", na.omit = FALSE, digits = 2, p.digits = 3, as.na = NULL,
           plot = FALSE, geom = c("all", "bar", "point", "ci", "jitter"),
           adjust = TRUE, filename = NULL, width = NA, height = NA, dpi = 600,
           write = NULL, append = TRUE, check = TRUE, output = TRUE)
}
\arguments{
\item{formula}{a formula of the form \code{y ~ group} where \code{y} is
a numeric variable giving the data values and \code{group}
a numeric variable, character variable or factor with two
or more than two values or factor levels giving the
corresponding groups.}

\item{data}{a matrix or data frame containing the variables in the
formula \code{formula}.}

\item{alternative}{a character string specifying the alternative hypothesis,
must be one of \code{"two.sided"} (default), \code{"greater"}
or \code{"less"}. Note that this argument is only used when
conducting Welch's two-sample t-test.}

\item{hypo}{logical: if \code{TRUE} (default), null and alternative
hypothesis are shown on the console.}

\item{descript}{logical: if \code{TRUE}, descriptive statistics are shown
on the console when conducting Welch's ANOVA.}

\item{effsize}{logical: if \code{TRUE}, effect size measure Cohen's d for
Welch's two-sample t-test (see \code{\link{cohens.d}}),
\eqn{\eta^2} and \eqn{\omega^2} for Welch's ANOVA and
Cohen's d for the post hoc tests are shown on the console.}

\item{weighted}{logical: if \code{TRUE}, the weighted pooled standard
deviation is used to compute Cohen's d.}

\item{correct}{logical: if \code{TRUE}, correction factor to remove
positive bias in small samples for is used to compute Cohen's d.}

\item{posthoc}{logical: if \code{TRUE}, Games-Howell post hoc test for
multiple comparison is conducted when performing Welch's ANOVA.}

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
to be used for displaying results.}

\item{p.digits}{an integer value indicating the number of decimal places
to be used for displaying the \emph{p}-value.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before
conducting the analysis.}

\item{plot}{logical: if \code{TRUE}, a plot showing bar plots with
error bars for confidence intervals is drawn. For
additional plotting arguments, see \code{Details} in
the help page of the function \code{plot.misty.object}.}

\item{geom}{a character string or character vector indicating which
plot elements to draw, i.e., \code{"all"} for drawing
all plot elements, \code{"bar"} for drawing bars representing
means for each groups, \code{"point"} for drawing points
representing means for each group, \code{"ci"} for drawing
error bars representing confidence intervals, and \code{"jitter"}
for drawing jittered data points.}

\item{adjust}{logical: if \code{TRUE} (default), difference-adjustment
for the confidence intervals is applied.}

\item{filename}{a character string indicating the \code{filename}
argument including the file extension in the \code{ggsave}
function. Note that one of \code{".eps"}, \code{".ps"},
\code{".tex"}, \code{".pdf"} (default),
\code{".jpeg"}, \code{".tiff"}, \code{".png"},
\code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
to be specified as file extension in the \code{filename}
argument. Note that plots can only be saved when
\code{plot = TRUE}.}

\item{width}{a numeric value indicating the \code{width} argument
(default is the size of the current graphics device)
in the \code{ggsave} function.}

\item{height}{a numeric value indicating the \code{height} argument
(default is the size of the current graphics device)
in the \code{ggsave} function.}

\item{dpi}{a numeric value indicating the \code{dpi} argument
(default: \code{600}) in the \code{ggsave} function.}

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
\item{\code{data}}{data frame with the outcome and grouping variable}
\item{\code{formula}}{formula}
\item{\code{args}}{specification of function arguments}
\item{\code{plot}}{ggplot2 object for plotting the results}
\item{\code{result}}{result table}
}
\description{
This function performs Welch's two-sample t-test and Welch's ANOVA including
Games-Howell post hoc test for multiple comparison and provides descriptive
statistics, effect size measures, and a plot showing bars representing means
for each group and error bars for difference-adjusted confidence intervals.
}
\details{
\describe{
\item{\strong{Effect Size Measure}}{By default, Cohen's d based on the non-weighted
standard deviation (i.e., \code{weighted = FALSE}) which does not assume homogeneity
of variance is computed (see Delacre et al., 2021) when requesting an effect size
measure (i.e., \code{effsize = TRUE}). Cohen's d based on the pooled standard
deviation assuming equality of variances between groups can be requested by
specifying \code{weighted = TRUE}.}
}
}
\examples{
#————————————————————————————————————————————————————————————————————————————
# Two-Sample Design

# Example 1a: Two-sided two-sample Welch-test
test.welch(hp ~ am, data = mtcars)

# Example 1b: One-sided two-sample Welch-test
test.welch(hp ~ am, data = mtcars, alternative = "less")

# Example 1c: Two-sided two-sample Welch-test,
# Print descriptive statistics and Cohen's d
test.welch(hp ~ am, data = mtcars, descript = TRUE, effsize = TRUE)

# Example 1d: Multiple two-sample Welch-test
test.welch(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars)

# Example 1e: Multiple two-sample Welch-test, bonferroni correction
test.welch(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars, p.adj = "bonferroni")

#————————————————————————————————————————————————————————————————————————————
# Multiple-Sample Design

# Example 2a: Welch's ANOVA
test.welch(hp ~ gear, data = mtcars)

# Example 2b: Welch's ANOVA,
# Print descriptive statistics and Games-Howell post hoc test
test.welch(hp ~ gear, data = mtcars, descript = TRUE, posthoc = TRUE)

# Example 2c: Welch's ANOVA, print eta-squared and omega-squared
test.welch(hp ~ gear, data = mtcars, effsize = TRUE)

#————————————————————————————————————————————————————————————————————————————
# Plot

# Example 3a: Plot results, default setting
test.welch(hp ~ gear, data = mtcars, plot = TRUE)

# Example 3b: Plot results
# No bars, draw points representing means and jittered data points
test.welch(hp ~ gear, data = mtcars, plot = TRUE, geom = c("point", "jitter"))

# Example 3c: Plot results using the plot() function, use additional arguments
# see Details in the help page of the function plot.misty.object
object <- test.welch(hp ~ gear, data = mtcars)
plot(object, geom = c("bar", "ci", "jitter"), jitter.alpha = 0.4, title = "Welch's Test")

#————————————————————————————————————————————————————————————————————————————
# Create Plot Manually

# Load ggplot2 package
library(ggplot2)

# Create misty object
object <- test.welch(hp ~ gear, data = mtcars)

# Example 4: Plot
ggplot(object$result$descript, aes(group, y)) +
  geom_bar(aes(group, m), stat = "summary", fun = "mean") +
  geom_errorbar(aes(group, m, ymin = low, ymax = upp), width = 0.1) +
  theme_bw()


\dontrun{
#————————————————————————————————————————————————————————————————————————————
# Write Results and Save Plot

# Example 5a: Write results into a text file
test.welch(hp ~ gear, data = mtcars, write = "Welch-Test.txt")

# Example 5a: Write results into an Excel file
test.welch(hp ~ gear, data = mtcars, write = "Welch-Test.xlsx")

# Example 5b: Save plot as PNG fine
test.welch(hp ~ gear, data = mtcars, plot = TRUE,
            filename = "Welch-Test.png", width = 6, height = 5)
}
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
- Using R and SPSS}. John Wiley & Sons.

Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021). Why Hedges' g*s
based on the non-pooled standard deviation should be reported with Welch's
t-test. https://doi.org/10.31234/osf.io/tu6mp
}
\seealso{
\code{\link{test.t}}, \code{\link{test.z}}, \code{\link{test.levene}},
\code{\link{aov.b}}, \code{\link{cohens.d}}, \code{\link{ci.mean.diff}},
\code{\link{ci.mean}}
}
\author{
Takuya Yanagida
}
