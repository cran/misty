\name{ci.var}
\alias{ci.var}
\alias{ci.sd}
\title{(Bootstrap) Confidence Intervals for Variances and Standard Deviations}
\usage{
ci.var(data, ..., method = c("chisq", "bonett"),
       boot = c("none", "perc", "bc", "bca"), nrep = 1000, seed = NULL,
       alternative = c("two.sided", "less", "greater"),
       conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
       na.omit = FALSE, digits = 2, as.na = NULL, plot = c("none", "ci", "boot"),
       geom = c("all", "hist", "density", "point", "ci", "line"),
       filename = NULL, width = NA, height = NA, dpi = 600, write = NULL,
       append = TRUE, check = TRUE, output = TRUE)

ci.sd(data, ..., method = c("chisq", "bonett"),
      boot = c("none", "perc", "bc", "bca"), nrep = 1000, seed = NULL,
      alternative = c("two.sided", "less", "greater"),
      conf.level = 0.95, group = NULL, split = NULL, sort.var = FALSE,
      na.omit = FALSE, digits = 2, as.na = NULL, plot = c("none", "ci", "boot"),
      geom = c("all", "hist", "density", "point", "ci", "line"),
      filename = NULL, width = NA, height = NA, dpi = 600, write = NULL,
      append = TRUE, check = TRUE, output = TRUE)
}
\arguments{
\item{data}{a numeric vector or data frame with numeric
variables, i.e., factors and character variables are
excluded from \code{data} before conducting the analysis.}

\item{...}{an expression indicating the variable names in \code{data},
e.g., \code{ci.var(dat, x1, x2, x3)}. Note that the
operators \code{+}, \code{-}, \code{~},
\code{:}, \code{::}, and \code{!} can also be used
to select variables, see 'Details' in the
\code{\link{df.subset}} function.}

\item{method}{a character string specifying the method for computing
the confidence interval, must be one of \code{"chisq"},
or \code{"bonett"} (default).}

\item{boot}{a character string specifying the type of bootstrap
confidence intervals (CI), i.e., \code{"none"} (default)
for not conducting bootstrapping, \code{"perc"}, for
the percentile bootstrap CI \code{"bc"} (default) for
the bias-corrected (BC) percentile bootstrap CI (without
acceleration), and \code{"bca"} for the bias-corrected
and accelerated (BCa) bootstrap CI, see 'Details' in
the \code{\link{ci.cor}} function.}

\item{nrep}{a numeric value indicating the number of bootstrap
replicates (default is 1000).}

\item{seed}{a numeric value specifying seeds of the pseudo-random
numbers used in the bootstrap algorithm when conducting
bootstrapping.}

\item{alternative}{a character string specifying the alternative hypothesis,
must be one of \code{"two.sided"} (default), \code{"greater"}
or \code{"less"}.}

\item{conf.level}{a numeric value between 0 and 1 indicating the confidence
level of the interval.}

\item{group}{either a character string indicating the variable name
of the grouping variable in \code{data}, or a vector representing the grouping
variable.}

\item{split}{either a character string indicating the variable name
of the split variable in 'data', or a vector representing the split variable.}

\item{sort.var}{logical: if \code{TRUE}, output table is sorted by
variables when specifying \code{group}.}

\item{na.omit}{logical: if \code{TRUE}, incomplete cases are removed
before conducting the analysis (i.e., listwise deletion)
when specifying more than one outcome variable.}

\item{digits}{an integer value indicating the number of decimal
places to be used.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before
conducting the analysis.
Note that \code{as.na()} function is only applied to
\code{data}, but not to \code{group} or \code{split}.}

\item{plot}{a character string indicating the type of the plot
to display, i.e., \code{"none"} (default) for not
displaying any plots, \code{"ci"} for displaying
confidence intervals for variances or standard deviations,
\code{"boot"} for displaying bootstrap samples with
histograms and density curves when the argument
\code{"boot"} is other than \code{"none"}.}

\item{geom}{a character string or character vector indicating which
plot elements to draw, i.e., \code{"all"} for drawing
all plot elements, \code{"hist"} for drawing histograms
when plotting bootstrap samples (\code{plot = "boot"}),
\code{"density"} for drawing density curves when plotting
bootstrap samples (\code{plot = "boot"}), \code{"point"}
for drawing vertical lines representing the point estimate
of the variance or standard deviation when plotting bootstrap
samples (\code{plot = "boot"}), \code{"ci"} for drawing
vertical lines representing the bootstrap confidence intervals
of the variance or standard deviation when plotting bootstrap
samples (\code{plot = "boot"}), and \code{"line"} for drawing
a horizontal line when specifying \code{plot = "ci"} or a
vertical line when specifying \code{plot = "boot"}.}

\item{filename}{a character string indicating the \code{filename}
argument including the file extension in the \code{ggsave}
function. Note that one of \code{".eps"}, \code{".ps"},
\code{".tex"}, \code{".pdf"} (default),
\code{".jpeg"}, \code{".tiff"}, \code{".png"},
\code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
to be specified as file extension in the \code{file}
argument. Note that plots can only be saved when
\code{plot = "ci"} or \code{plot = "boot"}.}

\item{width}{a numeric value indicating the \code{width} argument
(default is the size of the current graphics device)
in the \code{ggsave} function.}

\item{height}{a numeric value indicating the \code{height} argument
(default is the size of the current graphics device)
in the \code{ggsave} function.}

\item{dpi}{a numeric value indicating the \code{dpi} argument
(default is \code{600}) in the \code{ggsave} function.}

\item{write}{a character string naming a file for writing the output
into either a text file with file extension \code{".txt"}
(e.g., \code{"Output.txt"}) or Excel file with file
extension \code{".xlsx"}  (e.g., \code{"Output.xlsx"}).
If the file name does not contain any file extension,
an Excel file will be written.}

\item{append}{logical: if \code{TRUE} (default), output will be
appended to an existing text file with extension
\code{.txt} specified in \code{write}, if \code{FALSE}
existing text file will be overwritten.}

\item{check}{logical: if \code{TRUE} (default), argument specification
is checked.}

\item{output}{logical: if \code{TRUE} (default), output is shown
on the console.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:

\item{\code{call}}{function call}
\item{\code{type}}{type of analysis}
\item{\code{data}}{list with the input specified in \code{...}, \code{data}, \code{group}, and \code{split}}
\item{\code{args}}{specification of function arguments}
\item{\code{boot}}{data frame with bootstrap replicates of the variance or standard deviation when bootstrapping was requested}
\item{\code{plot}}{ggplot2 object for plotting the results and the data frame used for plotting}
\item{\code{result}}{result table}
}
\description{
The function \code{ci.var} computes and plots confidence intervals for variances,
and the function \code{ci.sd} computes confidence intervals for the standard
deviations, optionally by a grouping and/or split variable. These functions
also supports three types of bootstrap confidence intervals (e.g., bias-corrected
(BC) percentile bootstrap or bias-corrected and accelerated (BCa) bootstrap
confidence intervals) and plots the bootstrap samples with histograms and
density curves.
}
\details{
The confidence interval based on the chi-square distribution is computed by
specifying \code{method = "chisq"}, while the Bonett (2006) confidence interval
is requested by specifying \code{method = "bonett"}. By default, the Bonett
confidence interval interval is computed which performs well under moderate
departure from normality, while the confidence interval based on the chi-square
distribution is highly sensitive to minor violations of the normality assumption
and its performance does not improve with increasing sample size. Note that at
least four valid observations are needed to compute the Bonett confidence interval.
}
\note{
Bootstrap confidence intervals are computed using the R package \code{boot}
by Angelo Canty and Brain Ripley (2024).
}
\examples{
#————————————————————————————————————————————————————————————————————————————
# Confidence Interval (CI) for the Variance

# Example 1a: Two-Sided 95% CI
ci.var(mtcars)

# Example 1b: One-Sided 99% CI based on the chi-square distributio
ci.var(mtcars, alternative = "less", method = "chisq")

#————————————————————————————————————————————————————————————————————————————
# Confidence Interval (CI) for the Standard Deviation

# Example 2a: Two-Sided 95% CI
ci.sd(mtcars)

# Example 2b: One-Sided 99% CI based on the chi-square distribution
ci.sd(mtcars, alternative = "less", method = "chisq")

#————————————————————————————————————————————————————————————————————————————
# Grouping Variable

# Example 3a: Grouping variable
ci.var(mtcars, mpg, cyl, disp, group = "vs")

# Example 3b:  Alternative specification without using the '...' argument
ci.var(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs)

# Example 3c: Sort result tables by variables
ci.var(mtcars, mpg, cyl, disp, group = "vs", sort.var = TRUE)

#————————————————————————————————————————————————————————————————————————————
# Split Variable

# Example 4a: Split variable
ci.var(mtcars, mpg, cyl, disp, split = "am")

# Example 4b:  Alternative specification without using the '...' argument
ci.var(mtcars[, c("mpg", "cyl", "disp")], split = mtcars$am)

#————————————————————————————————————————————————————————————————————————————
# Grouping and Split Variable

# Example 5a: Grouping and split variable
ci.var(mtcars, mpg, cyl, disp, group = "vs", split = "am")

# Example 5b:  Alternative specification without using the '...' argument
ci.var(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs, split = mtcars$am)

# Example 5c: Sort result tables by variables
ci.var(mtcars, mpg, cyl, disp, group = "vs", split = "am", sort.var = TRUE)

\dontrun{
#————————————————————————————————————————————————————————————————————————————
# Bootstrap Confidence Interval (CI)

# Example 6a: Bias-corrected (BC) percentile bootstrap CI
ci.var(mtcars, boot = "bc")

# Example 6b: Bias-corrected and accelerated (BCa) bootstrap CI,
# 5000 bootstrap replications, set seed of the pseudo-random number generator
ci.var(mtcars, boot = "bca", nrep = 5000, seed = 42)

#————————————————————————————————————————————————————————————————————————————
# Plot Confidence Intervals

# Example 7a: Two-Sided 95% CI
ci.var(mtcars, plot = "ci")

# Example 7b: Grouping variable
ci.var(mtcars, disp, hp, group = "vs", plot = "ci")

# Example 7c: Split variable
ci.var(mtcars, disp, hp, split = "am", plot = "ci")

# Example 7d: Plot results using the plot() function, use additional arguments
# see Details in the help page of the function plot.misty.object
object <- ci.var(mtcars, disp, hp, plot = "ci")
plot(object, ybreaks = seq(0, 25000, by = 2500), title = "Confidence Intervals")

#————————————————————————————————————————————————————————————————————————————
# Plot Bootstrap Samples

# Example 8a: Two-Sided 95% CI
ci.var(mtcars, disp, hp, boot = "bc", plot = "boot")

# Example 8b: Grouping variable
ci.var(mtcars, disp, hp, group = "vs", boot = "bc", plot = "boot")

# Example 8c: Split variable
ci.var(mtcars, disp, hp, split = "am", boot = "bc", plot = "boot")

# Example 8d: Plot results using the plot() function, use additional arguments
# see Details in the help page of the function plot.misty.object
object <- ci.var(mtcars, disp, hp, boot = "bc", plot = "boot")
plot(object, fill = "gray30", title = "Bootstrap Samples")

#————————————————————————————————————————————————————————————————————————————
# Write Results and Save Plot

# Example 9a: Write results into a text file
ci.var(mtcars, disp, write = "CI_Var.txt")

# Example 9b: Write results into an Excel file
ci.var(mtcars, disp, write = "CI_Var.xlsx")

# Example 9c: Save plot as PNG file
ci.var(mtcars, disp, plot = "ci", filename = "CI_Var.png",
        width = 9, height = 6)
}
}
\references{
Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
- Using R and SPSS}. John Wiley & Sons.

Canty, A., & Ripley, B. (2024). \emph{boot: Bootstrap R (S-Plus) Functions}.
R package version 1.3-31.

Bonett, D. G. (2006). Approximate confidence interval for standard deviation
of nonnormal distributions. \emph{Computational Statistics and Data Analysis,
50}, 775-782. https://doi.org/10.1016/j.csda.2004.10.003
}
\seealso{
\code{\link{ci.mean}}, \code{\link{ci.mean.diff}}, \code{\link{ci.median}},
\code{\link{ci.prop}}, \code{\link{ci.prop.diff}},  \code{\link{ci.cor}},
\code{\link{descript}}
}
\author{
Takuya Yanagida
}
