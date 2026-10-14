\name{test.linear}
\alias{test.linear}
\title{Tests for Linearity in a Linear Model}
\usage{
test.linear(model, method = c("reset", "harv", "rain"), power = c(2, 3),
            frac = 0.5, order = c("none", "all", "mahalanobis"), p.adj = "none",
            digits = 2, p.digits = 3, plot = FALSE,
            geom = c("all", "none", "lm", "loess", "ci.lm", "ci.loess"),
            filename = NULL, width = NA, height = NA, dpi = 600,
            write = NULL, append = TRUE, check = TRUE, output = TRUE, ...)
}
\arguments{
\item{model}{a fitted model of class \code{lm}.}

\item{method}{a character string indicating the method used for testing
linearity, i.e., \code{"reset"} (default) for the regression
equation specification error (RESET) test, \code{"harv"}
for the Harvey-Collier test, and \code{"rain"} for the
rainbow test for linearity.}

\item{power}{a positive integer vector indicating the powers of the
fitted values that should be included in the auxiliary
regression model. By default, the test includes quadratic
and cubic terms in the model, i.e., \code{c(2, 3)}. Note
that this argument is only used for the RESET test.}

\item{frac}{a numeric value between 0 and 1 indicating the proportion
of observations used in the central subset model. Note that
this argument is only used for the rainbow test.}

\item{order}{a character string indicated how the observations are ordered,
i.e., \code{"none"} for observations assumed to be already
ordered, \code{"all"} for ordering each predictor in the model
specified in the \code{"model"} argument separately,
\code{"mahalanobis"} for ordering observations by their
Mahalanobis distances from the mean regressor. Note that
this argument is only used for the Harvey-Collier test and
the rainbow test.}

\item{p.adj}{a character string indicating an adjustment method for multiple
testing based on \code{\link{p.adjust}}, i.e., \code{none}
(default), \code{bonferroni}, \code{holm}, \code{hochberg},
\code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.}

\item{digits}{an integer value indicating the number of decimal places to
be used for displaying results.}

\item{p.digits}{an integer value indicating the number of decimal places to
be used for displaying the \emph{p}-value.}

\item{plot}{logical: if \code{TRUE}, partial residual plots or
component-plus-residual plots are drawn.
For additional plotting arguments, see \code{Details} in
the help page of the function \code{plot.misty.object}.}

\item{geom}{character vector indicating which plot elements to draw,
i.e., \code{"all"} (default) for drawing all plot elements,
\code{"none"} for drawing no regression or smoothing line,
\code{"lm"} for drawing the regression line, \code{"loess"}
for drawing the LOESS smoothing curve, \code{"ci.lm"} for
drawing confidence bands around the regression line,
\code{"ci.loess"} for drawing confidence bands around the
LOESS smoothing curve (i.e., \code{se = TRUE}).}

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

\item{output}{logical: if \code{TRUE} (default), output is shown.}

\item{...}{additional plotting arguments from the \code{plot.misty.object()}
function, see 'Details in the \code{\link{plot.misty.object}}
function.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:
\item{\code{call}}{function call}
\item{\code{type}}{type of analysis}
\item{\code{model}}{model specified in \code{model}}
\item{\code{plot}}{ggplot2 object for plotting the results}
\item{\code{args}}{specification of function arguments}
\item{\code{result}}{data frame with results}
}
\description{
This function performs the regression equation specification error (RESET)
test (Remsey, 1969), the Harvey-Collier test (Harvey & Collier, 1977), and
the rainbow test for linearity (Utts, 1982). By default, the function performs
the RESET test.
}
\note{
This function is based on the \code{resettest()}, \code{harvtest()},
and \code{raintest()} function in the \pkg{lmtest} package by Achim Zeileis
and Torsten Hothorn.
}
\examples{
\dontrun{
#————————————————————————————————————————————————————————————————————————————
# Tests for Linearity in a Linear Model

# Estimate linear model
mod <- lm(mpg ~ disp + hp + drat, data = mtcars)

# Example 1a: RESET test (default)
test.linear(mod)

# Example 1b: Harvey-Collier test
test.linear(mod, method = "harv")

# Example 1c: Rainbow test
test.linear(mod, method = "rain")

#————————————————————————————————————————————————————————————————————————————
# 'order' Argument

# Example 2a: Harvey-Collier test, order observations by 'hp'
test.linear(mod, method = "harv", order = "hp")

# Example 2a: Rainbow test, order by Mahalanobis distances
test.linear(mod, method = "rain", order = "mahalanobis")

#————————————————————————————————————————————————————————————————————————————
# Plot

# Example 3a: Partial residual plots
test.linear(mod, plot = TRUE)

# Example 3b: Plot with smoothing line only
test.linear(mod, plot = TRUE, geom = "loess")

# Example 3c: Partial residual plots, additional arguments
# see Details in the help page of the function plot.misty.object
test.linear(mod, plot = TRUE, point.size = 1, point.shape = 2)

# Example 3d: Plot results using the plot() function, additional arguments
# see Details in the help page of the function plot.misty.object
object <- test.linear(mod)
plot(object, point.size = 1, point.shape = 2)

#————————————————————————————————————————————————————————————————————————————
# Write Results

# Example 4a: Write results into a text file
test.linear(mod, write = "Test_Linear.txt")

# Example 4b: Write results into an Excel file
test.linear(mod, write = "Test_Linear.xlsx")

# Example 4c: Save plot as PNG fine
test.linear(mod, plot = TRUE, filename = "Linear.png", width = 15, height = 5)
}
}
\references{
Harvey, A., & Collier, P. (1977). Testing for functional misspecification in
regression analysis. \emph{Journal of Econometrics, 6}(1), 103-119.

Ramsey, J. B. (1969). Tests for specification errors in classical linear
least-squares regression analysis. \emph{Journal of the Royal Statistical Society: Series B (Methodological), 31}(2),
350-371.

Utts, J. M. (1982). The rainbow test for lack of fit in regression.
\emph{Communications in Statistics - Theory and Methods, 11}(24), 2801-2815.

Zeileis, A., & Hothorn, T. (2002). Diagnostic checking in regression relationships.
\emph{R News, 2}(3), 7-10.
}
\seealso{
\code{\link{check.resid}}, \code{\link{test.homo}}, \code{\link{test.norm}},
\code{\link{coeff.robust}}
}
\author{
Takuya Yanagida
}
