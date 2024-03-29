% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test.t.R
\name{test.t}
\alias{test.t}
\alias{test.t.default}
\alias{test.t.formula}
\title{t-Test}
\usage{
test.t(x, ...)

\method{test.t}{default}(x, y = NULL, mu = 0, paired = FALSE,
       alternative = c("two.sided", "less", "greater"),
       conf.level = 0.95,  hypo = TRUE, descript = TRUE, effsize = FALSE,
       weighted = FALSE, cor = TRUE, ref = NULL, correct = FALSE,
       plot = FALSE, point.size = 4, adjust = TRUE, error.width = 0.1,
       xlab = NULL, ylab = NULL, ylim = NULL, breaks = ggplot2::waiver(),
       line = TRUE, line.type = 3, line.size = 0.8,
       jitter = TRUE, jitter.size = 1.25, jitter.width = 0.05,
       jitter.height = 0, jitter.alpha = 0.1, title = "",
       subtitle = "Confidence Interval", digits = 2, p.digits = 4,
        as.na = NULL, write = NULL, append = TRUE,check = TRUE, output = TRUE, ...)

\method{test.t}{formula}(formula, data, alternative = c("two.sided", "less", "greater"),
       conf.level = 0.95, hypo = TRUE, descript = TRUE, effsize = FALSE,
       weighted = FALSE, cor = TRUE, ref = NULL, correct = FALSE,
       plot = FALSE, point.size = 4, adjust = TRUE, error.width = 0.1,
       xlab = NULL, ylab = NULL, ylim = NULL, breaks = ggplot2::waiver(),
       jitter = TRUE, jitter.size = 1.25, jitter.width = 0.05,
       jitter.height = 0, jitter.alpha = 0.1, title = "",
       subtitle = "Confidence Interval", digits = 2, p.digits = 4,
       as.na = NULL, write = NULL, append = TRUE, check = TRUE, output = TRUE, ...)
}
\arguments{
\item{x}{a numeric vector of data values.}

\item{...}{further arguments to be passed to or from methods.}

\item{y}{a numeric vector of data values.}

\item{mu}{a numeric value indicating the population mean under the
null hypothesis. Note that the argument \code{mu} is only
used when computing a one sample t-test.}

\item{paired}{logical: if \code{TRUE}, paired-samples t-test is computed.}

\item{alternative}{a character string specifying the alternative hypothesis,
must be one of \code{"two.sided"} (default),
\code{"greater"} or \code{"less"}.}

\item{hypo}{logical: if \code{TRUE} (default), null and alternative hypothesis
are shown on the console.}

\item{descript}{logical: if \code{TRUE} (default), descriptive statistics are shown
on the console.}

\item{effsize}{logical: if \code{TRUE}, effect size measure Cohen's d is
shown on the console, see \code{\link{cohens.d}} function.}

\item{weighted}{logical: if \code{TRUE}, the weighted pooled standard deviation
is used to compute Cohen's d for a two-sample design (i.e.,
\code{paired = FALSE}), while standard deviation of the
difference scores is used to compute Cohen's d for a
paired-sample design (i.e., \code{paired = TRUE}).}

\item{cor}{logical: if \code{TRUE} (default), \code{paired = TRUE},
and \code{weighted = FALSE}, Cohen's d for a paired-sample
design while controlling for the correlation between the
two sets of measurement is computed. Note that this
argument is only used in
a paired-sample design (i.e., \code{paired = TRUE}) when
specifying \code{weighted = FALSE}.}

\item{ref}{character string \code{"x"} or \code{"y"} for specifying
the reference reference group when using the default
\code{test.t()} function or a numeric value or character
string indicating the reference group in a two-sample
design when using the formula \code{test.t()} function.
The standard deviation of the reference variable or
reference group is used to standardized the mean difference
to compute Cohen's d. Note that this argument is only used
in a two-sample design (i.e., \code{paired = FALSE}).}

\item{correct}{logical: if \code{TRUE}, correction factor to remove
positive bias in small samples is used.}

\item{conf.level}{a numeric value between 0 and 1 indicating the confidence
level of the interval.}

\item{plot}{logical: if \code{TRUE}, a plot showing error bars for
confidence intervals is drawn.}

\item{point.size}{a numeric value indicating the \code{size} aesthetic for
the point representing the mean value.}

\item{adjust}{logical: if \code{TRUE} (default), difference-adjustment for the
confidence intervals in a two-sample design is applied.}

\item{error.width}{a numeric value indicating the horizontal bar width of
the error bar.}

\item{xlab}{a character string specifying the labels for the x-axis.}

\item{ylab}{a character string specifying the labels for the y-axis.}

\item{ylim}{a numeric vector of length two specifying limits of the
limits of the y-axis.}

\item{breaks}{a numeric vector specifying the points at which tick-marks
are drawn at the y-axis.}

\item{line}{logical: if \code{TRUE} (default), a horizontal line
is drawn at \code{mu} for the one-sample t-test or at
0 for the paired-sample t-test.}

\item{line.type}{an integer value or character string specifying the line
type for the line representing the population mean under
the null hypothesis, i.e., 0 = blank, 1 = solid, 2 = dashed,
3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash.}

\item{line.size}{a numeric value indicating the \code{linewidth} aesthetic
for the line representing the population mean under the
null hypothesis.}

\item{jitter}{logical: if \code{TRUE} (default), jittered data points
are drawn.}

\item{jitter.size}{a numeric value indicating the \code{size} aesthetic}

\item{jitter.width}{a numeric value indicating the amount of horizontal jitter.}

\item{jitter.height}{a numeric value indicating the amount of vertical jitter.}

\item{jitter.alpha}{a numeric value indicating the opacity of the jittered
data points.}

\item{title}{a character string specifying the text for the title for
the plot.}

\item{subtitle}{a character string specifying the text for the subtitle for
the plot.}

\item{digits}{an integer value indicating the number of decimal places
to be used for displaying descriptive statistics and
confidence interval.}

\item{p.digits}{an integer value indicating the number of decimal places
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

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}

\item{output}{logical: if \code{TRUE} (default), output is shown on the console.}

\item{formula}{in case of two sample t-test (i.e., \code{paired = FALSE}),
a formula of the form \code{y ~ group} where \code{group}
is a numeric variable, character variable or factor with
two values or factor levels giving the corresponding
groups.}

\item{data}{a matrix or data frame containing the variables in the
formula \code{formula}.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:
  \item{\code{call}}{function call}
  \item{\code{type}}{type of analysis}
  \item{\code{sample}}{type of sample, i.e., one-, two-, or paired sample}
  \item{\code{formula}}{formula of the current analysis}
  \item{\code{data}}{data frame specified in \code{data}}
  \item{\code{plot}}{ggplot2 object for plotting the results}
  \item{\code{args}}{specification of function arguments}
  \item{\code{result}}{result table}
}
\description{
This function performs one-sample, two-sample, and paired-sample t-tests and
provides descriptive statistics, effect size measure, and a plot showing error
bars for (difference-adjusted) confidence intervals with jittered data points.
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
dat1 <- data.frame(group = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
                   x = c(3, 1, 4, 2, 5, 3, 2, 3, 6, 6, 3, NA))

#-------------------------------------------------------------------------------
# One-Sample Design

# Example 1a: Two-sided one-sample t-test
# population mean = 3
test.t(dat1$x, mu = 3)

# Example 1b: One-sided one-sample t-test
# population mean = 3, population standard deviation = 1.2
test.t(dat1$x, mu = 3, alternative = "greater")

# Example 1c: Two-sided one-sample t-test
# population mean = 3, convert value 3 to NA
test.t(dat1$x, mu = 3, as.na = 3)

# Example 1d: Two-sided one-sample t-test
# population mean = 3, print Cohen's d
test.t(dat1$x, sigma = 1.2, mu = 3, effsize = TRUE)

# Example 1e: Two-sided one-sample t-test
# population mean = 3, print Cohen's d with small sample correction factor
test.t(dat1$x, sigma = 1.2, mu = 3, effsize = TRUE, correct = TRUE)

# Example 1f: Two-sided one-sample t-test
# population mean = 3,
# do not print hypotheses and descriptive statistics
test.t(dat1$x, sigma = 1.2, mu = 3, hypo = FALSE, descript = FALSE)

# Example 1g: Two-sided one-sample t-test
# print descriptive statistics with 3 digits and p-value with 5 digits
test.t(dat1$x,  mu = 3, digits = 3, p.digits = 5)

\dontrun{
# Example 1h: Two-sided one-sample t-test
# population mean = 3, plot results
test.t(dat1$x, mu = 3, plot = TRUE)

# Load ggplot2 package
library(ggplot2)

# Save plot, ggsave() from the ggplot2 package
ggsave("One-sample_t-test.png", dpi = 600, width = 3, height = 6)

# Example 1i: Two-sided one-sample t-test
# population mean = 3, extract plot
p <- test.t(dat1$x, mu = 3, output = FALSE)$plot
p

# Extract data
plotdat <- data.frame(x = test.t(dat1$x, mu = 3, output = FALSE)$data[[1]])

# Draw plot in line with the default setting of test.t()
ggplot(plotdat, aes(0, x)) +
   geom_point(stat = "summary", fun = "mean", size = 4) +
   stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.20) +
   scale_x_continuous(name = NULL, limits = c(-2, 2)) +
   scale_y_continuous(name = NULL) +
   geom_hline(yintercept = 3, linetype = 3, linewidth = 0.8) +
   labs(subtitle = "Two-Sided 95% Confidence Interval") +
   theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank())
}
#-------------------------------------------------------------------------------
# Two-Sample Design

# Example 2a: Two-sided two-sample t-test
test.t(x ~ group, data = dat1)

# Example 2b: One-sided two-sample t-test
test.t(x ~ group, data = dat1, alternative = "greater")

# Example 2c: Two-sided two-sample t-test
# print Cohen's d with weighted pooled SD
test.t(x ~ group, data = dat1, effsize = TRUE)

# Example 2d: Two-sided two-sample t-test
# print Cohen's d with unweighted pooled SD
test.t(x ~ group, data = dat1, effsize = TRUE, weighted = FALSE)

# Example 2e: Two-sided two-sample t-test
# print Cohen's d with weighted pooled SD and
# small sample correction factor
test.t(x ~ group, data = dat1, effsize = TRUE, correct = TRUE)

# Example 2f: Two-sided two-sample t-test
# print Cohen's d with SD of the reference group 1
test.t(x ~ group, data = dat1, effsize = TRUE,
       ref = 1)

# Example 2f: Two-sided two-sample t-test
# print Cohen's d with weighted pooled SD and
# small sample correction factor
test.t(x ~ group, data = dat1, effsize = TRUE,
       correct = TRUE)

# Example 2h: Two-sided two-sample t-test
# do not print hypotheses and descriptive statistics,
test.t(x ~ group, data = dat1, descript = FALSE, hypo = FALSE)

# Example 2i: Two-sided two-sample t-test
# print descriptive statistics with 3 digits and p-value with 5 digits
test.t(x ~ group, data = dat1, digits = 3, p.digits = 5)

\dontrun{
# Example 2j: Two-sided two-sample t-test
# Plot results
test.t(x ~ group, data = dat1, plot = TRUE)

# Load ggplot2 package
library(ggplot2)

# Save plot, ggsave() from the ggplot2 package
ggsave("Two-sample_t-test.png", dpi = 600, width = 4, height = 6)

# Example 2k: Two-sided two-sample t-test
# extract plot
p <- test.t(x ~ group, data = dat1, output = FALSE)$plot
p

# Extract data used to plot results
plotdat <- test.t(x ~ group, data = dat1, output = FALSE)$data

# Draw plot in line with the default setting of test.t()
ggplot(plotdat, aes(factor(group), x)) +
   geom_point(stat = "summary", fun = "mean", size = 4) +
   stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.20) +
   scale_x_discrete(name = NULL) + scale_y_continuous(name = "y") +
   labs(title = "", subtitle = "Two-Sided 95% Confidence Interval") +
   theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5))
}

#-----------------

group1 <- c(3, 1, 4, 2, 5, 3, 6, 7)
group2 <- c(5, 2, 4, 3, 1)

# Example 2l: Two-sided two-sample t-test
test.t(group1, group2)

#-------------------------------------------------------------------------------
# Paired-Sample Design

dat2 <- data.frame(pre = c(1, 3, 2, 5, 7),
                   post = c(2, 2, 1, 6, 8))

# Example 3a: Two-sided paired-sample t-test
test.t(dat2$pre, dat2$post, paired = TRUE)

# Example 3b: One-sided paired-sample t-test
test.t(dat2$pre, dat2$post, paired = TRUE,
       alternative = "greater")

# Example 3c: Two-sided paired-sample t-test
# convert value 1 to NA
test.t(dat2$pre, dat2$post, as.na = 1, paired = TRUE)

# Example 3d: Two-sided paired-sample t-test
# print Cohen's d based on the standard deviation of the difference scores
test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE)

# Example 3e: Two-sided paired-sample t-test
# print Cohen's d based on the standard deviation of the difference scores
# with small sample correction factor
test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
       correct = TRUE)

# Example 3f: Two-sided paired-sample t-test
# print Cohen's d controlling for the correlation between measures
test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
       weighted = FALSE)

# Example 3g: Two-sided paired-sample t-test
# print Cohen's d controlling for the correlation between measures
# with small sample correction factor
test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
       weighted = FALSE, correct = TRUE)

# Example 3h: Two-sided paired-sample t-test
# print Cohen's d ignoring the correlation between measures
test.t(dat2$pre, dat2$post, paired = TRUE, effsize = TRUE,
       weighted = FALSE, cor = FALSE)

# Example 3i: Two-sided paired-sample t-test
# do not print hypotheses and descriptive statistics
test.t(dat2$pre, dat2$post, paired = TRUE, hypo = FALSE, descript = FALSE)

# Example 3j: Two-sided paired-sample t-test
# population standard deviation of difference score = 1.2
# print descriptive statistics with 3 digits and p-value with 5 digits
test.t(dat2$pre, dat2$post, paired = TRUE, digits = 3,
       p.digits = 5)

\dontrun{
# Example 3k: Two-sided paired-sample t-test
# Plot results
test.t(dat2$pre, dat2$post, paired = TRUE, plot = TRUE)

# Load ggplot2 package
library(ggplot2)

# Save plot, ggsave() from the ggplot2 package
ggsave("Paired-sample_t-test.png", dpi = 600, width = 3, height = 6)

# Example 3l: Two-sided paired-sample t-test
# Extract plot
p <- test.t(dat2$pre, dat2$post, paired = TRUE, output = FALSE)$plot
p

# Extract data used to plot results
plotdat <- data.frame(test.t(dat2$pre, dat2$post, paired = TRUE, output = FALSE)$data)

# Difference score
plotdat$diff <- plotdat$y - plotdat$x

# Draw plot in line with the default setting of test.t()
ggplot(plotdat, aes(0, diff)) +
   geom_point(stat = "summary", fun = "mean", size = 4) +
   stat_summary(fun.data = "mean_cl_normal", geom = "errorbar", width = 0.20) +
   scale_x_discrete(name = NULL) + scale_y_continuous(name = NULL) +
   geom_hline(yintercept = 0, linetype = 3, linewidth = 0.8) +
   labs(subtitle = "Two-Sided 95% Confidence Interval") +
   theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5),
                      axis.text.x = element_blank(),
                      axis.ticks.x = element_blank())
}
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in
psychology - Using R and SPSS}. John Wiley & Sons.

Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021). Why Hedges' g*s
based on the non-pooled standard deviation should be reported with Welch's t-test.
https://doi.org/10.31234/osf.io/tu6mp
}
\seealso{
\code{\link{aov.b}}, \code{\link{aov.w}}, \code{\link{test.welch}}, \code{\link{test.z}},
\code{\link{test.levene}}, \code{\link{cohens.d}}, \code{\link{ci.mean.diff}},
\code{\link{ci.mean}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
