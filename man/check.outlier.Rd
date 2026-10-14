\name{check.outlier}
\alias{check.outlier}
\title{Outlier and Influential Observations Diagnostics for Linear Models}
\usage{
check.outlier(model, type = c("mahal", "hat", "rstand", "rstud", "cook", "dfb"),
              cutoff = NULL, sample = 0.10, sort = TRUE, append = TRUE,
              plot = TRUE, geom = c("none", "text"), filename = NULL,
              width = NA, height = NA, dpi = 600, check = TRUE, ...)
}
\arguments{
\item{model}{a fitted model of class \code{"lm"}.}

\item{type}{a character vector indicating which statistical measures to
include in the plot, i.e., \code{"mahal"} for the Mahalanobis
distance, \code{"hat"} for the hat value, \code{"rstand"}
for the standardized leverage-corrected residual, \code{"rstud"}
for the studentized leverage-corrected residual, \code{"cook"}
for the Cook's distance, and \code{"dfb"} for the DFBETAS.}

\item{cutoff}{a named numeric vector indicating the cutoff values for the
statistical measures shown in the plot as horizontal lines.
Note that the elements of the vectors need to be need with
\code{"mahal"}, \code{"hat"}, \code{"rstand"}, \code{"rstud"},
\code{"cook"}, and \code{"dfb"}. For example,
\code{cutoff = c(hat = 0.2, cook = 0.3)} to draw
a horizontal line at \code{0.2} for the hat value plot and a
horizontal line at \code{0.3} for the Cook's distance plot.}

\item{sample}{numeric value for selecting the sample with the highest
absolute values in the statistical measures for leverage, distance,
and influence. The value is either a proportion between 0 and 1,
e.g., \code{sample = 0.2} for the 20\% highest absolute values,
or an integer value for selecting the number of observations
with the highest absolute values. Note that the logical argument
\code{FALSE} can be used to plot all observations. By default,
the function plots observations with the 10\% highest absolute
values, but a minimum of 10 observations.}

\item{sort}{logical: if \code{TRUE} (default), observations are sorted
in decreasing order by the statistical measure.}

\item{append}{logical: logical: if \code{TRUE} (default), statistical measures for
leverage, distance, and influence are appended to the data frame in \code{model$model}.}

\item{plot}{logical: if \code{TRUE} (default), a plot is drawn.}

\item{geom}{a character vector indicating which plot elements to draw, i.e.,
\code{"none"} for not drawing any labels, \code{"text"} (default)
for drawing labels above or below each observation.}

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

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}

\item{...}{further arguments to be passed to or from methods.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:

\item{\code{call}}{function call}
\item{\code{type}}{type of analysis}
\item{\code{model}}{model specified in \code{model }}
\item{\code{args}}{specification of function arguments}
\item{\code{plot}}{ggplot2 object for plotting the residuals}
\item{\code{data}}{list with data frame, i.e., \code{data} for a data frame
                   in \code{model$model} with statistical measures for leverage,
                   distance, and influence are appended to the data frame when
                   \code{append = TRUE}, \code{mahal} for the sample of \code{data}
                   with the highest Mahalanobis distance, \code{hat} for the
                   sample of \code{data} with the highest hat values, \code{rstand}
                   for the sample of \code{data} with the highest absolute
                   standardized leverage-corrected residuals, \code{rstud} for
                   the sample  of \code{data} with the highest absolute studentized
                   leverage-corrected residuals, \code{cook} for the sample of
                   \code{data} with  the highest Cook's distance, and \code{*.dfb}
                   for the sample of \code{data} with the highest absolute DFBETAS}
}
\description{
This function performs outlier and influential observations diagnostics by plotting
for statistical measures for leverage, distance, and influence for linear models
estimated by using the \code{lm()} function. \emph{Mahalanobis distance} and
\emph{hat values} are computed for quantifying \strong{leverage},
\emph{standardized leverage-corrected residuals} and \emph{studentized leverage-corrected residuals}
are computed for quantifying \strong{distance}, and \emph{Cook's distance} and
standardized \emph{DFBETAS} are computed for quantifying \strong{influence}.
By default, the function provides plots showing observations with the 10\%
highest absolute values or a minimum of 10 observations with the highest absolute
values in the \emph{hat values}, \emph{studentized leverage-corrected residuals},
and \emph{Cook's distances}.
}
\details{
In regression analysis, an observation can be extreme in three major ways (see
Darlington & Hayes, 2017, p. 484): (1) An observation has high \strong{leverage} if it
has a atypical pattern of values on the predictors, (2) an observation has high
\strong{distance} if its observed outcome value \eqn{Y_i} has a large deviation
from the predicted value \eqn{\hat{Y}_i}, and (3) an observation has high
\strong{influence} if its inclusion substantially changes the estimates for the
intercept and/or slopes.
}
\examples{
\dontrun{
#————————————————————————————————————————————————————————————————————————————
# Outlier and Influential Observations Diagnostics for Linear Models

# Estimate linear model
mod <- lm(mpg ~ cyl + disp + hp, data = mtcars)

#——————————————————————————————————————
## Default Setting

# Example 1a: Hat values, studentized leverage-corrected residuals, and Cook's distance
check.outlier(mod)

# Example 1b: Additional plot arguments
# see Details in the help page of the function plot.misty.object
check.outlier(mod, point.shape = 1, point.size = 4, line.col = "red")

#——————————————————————————————————————
## Argument 'cutoff' and 'type'

# Example 2a: Horizontal cutoff lines
check.outlier(mod, cutoff = c(hat = 0.3, rstud = 2, cook = 0.1))

# Example 2b: Mahalanobis distance and standardized leverage-corrected residuals
check.outlier(mod, type = c("mahal", "rstand"), cutoff = c(mahal = 8, rstand = 2))

# Example 2c: DFBETAS
check.outlier(mod, type = "dfb", cutoff = c(dfb = 0.35))

#——————————————————————————————————————
## Arguments 'sample' and 'sort'

# Example 3a: Plot entire data set and do not sort
check.outlier(mod, sample = FALSE, sort = FALSE)

# Example 3b: Plot 50\% highest absolute values
check.outlier(mod, sample = 0.5)

# Example 3c: Plot n = 15 highest absolute values
check.outlier(mod, sample = 15)

#——————————————————————————————————————
## Data

object <- check.outlier(mod)

# Example 4a: Data frame with with statistical measures
object$data$data

# Example 4b: Sample of data with the highest hat values
object$data$hat

#————————————————————————————————————————————————————————————————————————————
# Save Plot

# Example 7: Save plot as PNG fine
check.outlier(mod, filename = "Outlier.png", width = 12, height = 15)
}
}
\references{
Darlington, R. B., &, Hayes, A. F. (2017). \emph{Regression analysis and linear
models}: Concepts, applications, and implementation. The Guilford Press.
}
\seealso{
\code{\link{check.resid}}, \code{\link{check.collin}}
}
\author{
Takuya Yanagida
}
