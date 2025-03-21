% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test.levene.R
\name{test.levene}
\alias{test.levene}
\title{Levene's Test for Homogeneity of Variance}
\usage{
test.levene(formula, data, method = c("median", "mean"), conf.level = 0.95,
            hypo = TRUE, descript = TRUE, digits = 2, p.digits = 3, as.na = NULL,
            plot = FALSE, violin = TRUE, box = TRUE, jitter = FALSE,
            violin.alpha = 0.3, violin.trim = FALSE, box.alpha = 0.2,
            box.width = 0.2, jitter.size = 1.25, jitter.width = 0.05,
            jitter.height = 0, jitter.alpha = 0.2, gray = FALSE, start = 0.9,
            end = 0.4, color = NULL, xlab = NULL, ylab = NULL, ylim = NULL,
            ybreaks = ggplot2::waiver(), title = "", subtitle = "",
            filename = NULL, width = NA, height = NA,
            units = c("in", "cm", "mm", "px"), dpi = 600, write = NULL,
            append = TRUE, check = TRUE, output = TRUE)
}
\arguments{
\item{formula}{a formula of the form \code{y ~ group} where \code{y} is
a numeric variable giving the data values and \code{group}
a numeric variable, character variable or factor with two
or more than two values or factor levels giving the
corresponding groups.}

\item{data}{a matrix or data frame containing the variables in the
formula \code{formula}.}

\item{method}{a character string specifying the method to compute the
center of each group, i.e. \code{method = "median"} (default)
to compute the Levene's test based on the median (aka
Brown-Forsythe test) or \code{method = "mean"} to compute
the Levene's test based on the arithmetic mean.}

\item{conf.level}{a numeric value between 0 and 1 indicating the confidence
level of the interval.}

\item{hypo}{logical: if \code{TRUE} (default), null and alternative hypothesis
are shown on the console.}

\item{descript}{logical: if \code{TRUE} (default), descriptive statistics are shown
on the console.}

\item{digits}{an integer value indicating the number of decimal places
to be used for displaying results.}

\item{p.digits}{an integer value indicating the number of decimal places
to be used for displaying the \emph{p}-value.}

\item{as.na}{a numeric vector indicating user-defined missing values,
i.e. these values are converted to \code{NA} before
conducting the analysis.}

\item{plot}{logical: if \code{TRUE}, a plot showing violins with
boxplots is drawn.}

\item{violin}{logical: if \code{TRUE} (default), violins are drawn.}

\item{box}{logical: if \code{TRUE} (default), boxplots are drawn.}

\item{jitter}{logical: if \code{TRUE} (default), jittered data points
are drawn.}

\item{violin.alpha}{a numeric value between 0 and 1 for specifying the
\code{alpha} argument in the \code{geom_violin}
function for controlling the opacity of the violins.}

\item{violin.trim}{logical: if \code{TRUE}, the tails of the violins to the
range of the data is trimmed.}

\item{box.alpha}{a numeric value between 0 and 1 for specifying the
\code{alpha} argument in the \code{geom_boxplot}
function for controlling the opacity of the boxplots.}

\item{box.width}{a numeric value indicating the width of the boxplots.}

\item{jitter.size}{a numeric value indicating the \code{size} aesthetic
for the jittered data points.}

\item{jitter.width}{a numeric value indicating the amount of horizontal jitter.}

\item{jitter.height}{a numeric value indicating the amount of vertical jitter.}

\item{jitter.alpha}{a numeric value between 0 and 1 for specifying the
\code{alpha} argument in the \code{geom_jitter}
function for controlling the opacity of the jittered
data points.}

\item{gray}{logical: if \code{TRUE}, the plot is drawn in gray scale.}

\item{start}{a numeric value between 0 and 1, graphical parameter to
specify the gray value at the low end of the palette.}

\item{end}{a numeric value between 0 and 1, graphical parameter to
specify the gray value at the high end of the palette.}

\item{color}{a character vector, indicating the color of the violins
and the boxes. By default, default ggplot2 colors are
used.}

\item{xlab}{a character string specifying the labels for the x-axis.}

\item{ylab}{a character string specifying the labels for the y-axis.}

\item{ylim}{a numeric vector of length two specifying limits of the
limits of the y-axis.}

\item{ybreaks}{a numeric vector specifying the points at which tick-marks
are drawn at the y-axis.}

\item{title}{a character string specifying the text for the title for
the plot.}

\item{subtitle}{a character string specifying the text for the subtitle
for the plot.}

\item{filename}{a character string indicating the \code{filename} argument (default is \code{"NA_Pattern.pdf"}) including
the file extension for the \code{ggsave} function. Note that one of \code{".eps"}, \code{".ps"},
\code{".tex"}, \code{".pdf"} (default), \code{".jpeg"}, \code{".tiff"}, \code{".png"}, \code{".bmp"},
\code{".svg"} or \code{".wmf"} needs to be specified as file extension in the \code{file} argument.}

\item{width}{a numeric value indicating the \code{width} argument (default is the
size of the current graphics device) for the \code{ggsave} function.}

\item{height}{a numeric value indicating the \code{height} argument
(default is the size of the current graphics device) for the \code{ggsave} function.}

\item{units}{a character string indicating the \code{units} argument
(default is \code{in}) for the \code{ggsave} function.}

\item{dpi}{a numeric value indicating the \code{dpi} argument
(default is \code{600}) for the \code{ggsave} function.}

\item{write}{a character string naming a text file with file extension
\code{".txt"} (e.g., \code{"Output.txt"}) for writing the
output into a text file.}

\item{append}{logical: if \code{TRUE} (default), output will be appended
to an existing text file with extension \code{.txt} specified
in \code{write}, if \code{FALSE} existing text file will be
overwritten.}

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}

\item{output}{logical: if \code{TRUE} (default), output is shown.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:
  \item{\code{call}}{function call}
  \item{\code{type}}{type of analysis}
  \item{\code{data}}{data frame specified in \code{data}}
  \item{\code{formula}}{formula of the current analysis}
  \item{\code{args}}{specification of function arguments}
  \item{\code{plot}}{ggplot2 object for plotting the results}
  \item{\code{result}}{list with result tables, i.e., \code{descript} for
                       descriptive statistics and \code{test} for the ANOVA
                       table}
}
\description{
This function performs Levene's test for homogeneity of variance across two
or more independent groups including a plot showing violins and boxplots
representing the distribution of the outcome variable for each group.
}
\details{
Levene's test is equivalent to a one-way analysis of variance (ANOVA) with the
absolute deviations of observations from the mean of each group as dependent
variable (\code{center = "mean"}). Brown and Forsythe (1974) modified the
Levene's test by using the absolute deviations of observations from the median
(\code{center = "median"}). By default, the Levene's test uses the absolute
deviations of observations from the median.
}
\examples{
# Example 1: Levene's test based on the median
test.levene(mpg ~ gear, data = mtcars)

# Example 2: Levene's test based on the arithmetic mean
test.levene(mpg ~ gear, data = mtcars, method = "mean")

# Example 3: Levene's test based on the median, plot results
test.levene(mpg ~ gear, data = mtcars, plot = TRUE)

\dontrun{
# Example 4: Write results into a text file
test.levene(mpg ~ gear, data = mtcars, write = "Levene.txt")

# Example 5: Levene's test based on the median, save plot
test.levene(mpg ~ gear, data = mtcars, plot = TRUE,
            filename = "Levene-test.png", dpi = 600, width = 6, height = 5)}
}
\references{
Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of
variances. \emph{Journal of the American  Statistical Association, 69},
364-367.

Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
- Using R and SPSS}. John Wiley & Sons.
}
\seealso{
\code{\link{aov.b}}, \code{\link{test.t}}, \code{\link{test.welch}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
