\name{test.homo}
\alias{test.homo}
\title{Tests for Homoscedasticity in a Linear Model}
\usage{
test.homo(model, method = c("bp", "sbp", "white1", "white2", "evansking", "verbyla"),
          pred = attr(terms(model), which = "term.labels"), lambda.star = 5,
          digits = 2, p.digits = 3,
          plot = FALSE, geom = c("all", "none", "hline", "loess", "ci.loess"),
          filename = NULL, width = NA, height = NA, dpi = 600,
          write = NULL, append = TRUE, check = TRUE, output = TRUE, ...)
}
\arguments{
\item{model}{a fitted model of class \code{lm}.}

\item{method}{a character string indicating the method used for testing
homoscedasticity, i.e., \code{"bp"} for the the Breusch-Pagan
test, \code{"sbp"} for the Koenker's Studentized
Breusch-Pagan test, \code{"white1"} for the White's Test
regressing squared residuals on predictors and their squares,
\code{"white2"} for the White's Test regressing squared
residuals on predictors, their squares, and two-way interactions,
\code{"evansking"} (default) for the Evans-King Generalized Least
Squares test, and \code{"verbyla"} for the Verbyla's Residual
Maximum Likelihood test.}

\item{pred}{a character string or character vector indicating the
predictor variables for the auxiliary design matrix that
are suspected to be related to the residual variance under
the alternative hypothesis. By default, the design matrix
of the regression model including all predictor variables
is used.}

\item{lambda.star}{a numeric value (default: \code{5}) indicating the degree
of heteroskedasticity under the alternative hypothesis for
Evans-King Generalized Least Squares test (\code{method = "evansking"}).
Evans and  King (1985) suggests 2.5, 5, 7.5, and 10 as values
to consider, and Evans and King (1988) finds that 2.5 and 5
perform best empirically.}

\item{plot}{logical: if \code{TRUE}, a plot for predicted values against
the studentized residuals is drawn.
For additional plotting arguments, see \code{Details} in
the help page of the function \code{plot.misty.object}.}

\item{geom}{a character vector indicating which plot elements to draw,
i.e., \code{"all"} (default) for drawing all plot elements,
\code{"none"} for drawing no horizontal or smoothing line,
\code{"hline"} for drawing a horizontal line at \code{0},
\code{"loess"} for drawing the LOESS smoothing curve, and
\code{"ci.loess"} for drawing confidence bands around the
smoothing curve (i.e., \code{se = TRUE}).}

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

\item{digits}{an integer value indicating the number of decimal places
to be used for displaying the test statistic.}

\item{p.digits}{an integer value indicating the number of decimal places
to be used for displaying the \emph{p}-value.}

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
This function performs the Breusch-Pagan test (Breusch & Pagan, 1979) without
or with the studentising modification of Koenker (1981), the White's test
with squared terms or squared and interaction terms (White, 1980), Evans-King
Generalized Least Squares (GLS) test (Evans & King, 1988), and the Verbyla's Residual
Maximum Likelihood test (Verbyla, 1993) for testing homoskedasticity (null
hypothesis) against heteroskedasticity (alternative hypothesis) in a linear
regression model. By default, the function performs the Evans-King GLS test.
}
\details{
Farrar et al. (2025) reviewed 25 different tests for homoscedasticity implemented
in the R package \pkg{scedastic} (Farrar, 2025) and presented results of a Monte
Carlo simulation demonstrating that the Evans-King GLS test and Verbyla's
Residual Maximum Likelihood test outperform commonly used tests for homoskedasticity,
such as the Koenker's Studentized Breusch-Pagan test, White’s test, and the
Goldfeld-Quandt F test.
}
\note{
This function is based on the \code{breusch_pagan()}, \code{white()},
\code{evans_king()}, \code{pRQF()} and \code{verbyla()} function
in the \pkg{skedastic} package by Thomas Farrar.
}
\examples{
\dontrun{
#————————————————————————————————————————————————————————————————————————————
# Tests for Homoscedasticity in a Linear Model

# Estimate linear model
mod <- lm(mpg ~ cyl + disp, data = mtcars)

# Example 1a: Koenker's Studentized Breusch-Pagan test (default)
test.homo(mod)

# Example 1b: White's Test regressing squared residuals on predictors and their squares
test.homo(mod, method = "white1")

# Example 1c: Evans-King Generalized Least Squares test
test.homo(mod, method = "evansking")

# Example 1d: Verbyla's Residual Maximum Likelihood test
test.homo(mod, method = "verbyla")

# Example 2: Predictor variable 'disp' used in the auxiliary design matrix
test.homo(mod, pred = "disp")

#————————————————————————————————————————————————————————————————————————————
# Plot

# Example 3a: Predicted values vs. Studentized residuals
test.homo(mod, plot = TRUE)

# Example 3b: Plot with horizontal line only
test.homo(mod, plot = TRUE, geom = "hline")

# Example 3c: redicted values vs. Studentized residuals, additional arguments
# see Details in the help page of the function plot.misty.object
test.homo(mod, plot = TRUE, point.shape = 2, linetype = "solid", line.col = "blue",
          xlim = c(0, 30))

# Example 3d: Plot results using the plot() function, use additional arguments
# see Details in the help page of the function plot.misty.object
object <- test.homo(mod)
plot(object, point.shape = 2, linetype = "solid", line.col = "blue", xlim = c(0, 30))

#————————————————————————————————————————————————————————————————————————————
# Write Results

# Example 4a: Write results into a text file
test.homo(mod, write = "Test_Homo.txt")

# Example 4b: Write results into an Excel file
test.homo(mod, write = "Test_Homo.xlsx")

# Example 4c: Save plot as PNG fine
test.homo(mod, plot = TRUE, filename = "Homo.png", width = 7, height = 6)
}
}
\references{
Breusch, T. S., & Pagan, A.R. (1979) A simple test for heteroscedasticity and
Random Coefficient Variation. \emph{Econometrica, 47}, 1287-1294.
http://dx.doi.org/10.2307/1911963

Evans, M. A., & King, M. L. (1985). A point optimal test for heteroscedastic
disturbances. \emph{Journal of Econometrics, 27}(2), 163–178.

Evans, M. A., & King, M. L. (1988). A further class of tests for heteroscedasticity.
\emph{Journal of Econometrics, 37}(2), 265–276.

Farrar, T. J. (2025). \emph{skedastic: Heteroskedasticity Diagnostics for Linear Regression Models}.
R Package version 2.0.3. https://github.com/tjfarrar/skedastic

Farrar, T., Blignaut, R., Luus, R., & Steel, S. (2025). A review and comparison
of methods of testing for heteroskedasticity in the linear regression model.
\emph{Journal of applied statistics, 52}(16), 3121-3150.
https://doi.org/10.1080/02664763.2025.2575038

Koenker, R. (1981). A note on studentizing a test for heteroscedasticity.
\emph{Journal of Econometrics, 17}(1), 107–112.
https://doi.org/10.1016/0304-4076(81)90062-2.

Verbyla, A. P. (1993). Modelling variance heterogeneity: Residual maximum
likelihood and diagnostics. \emph{Journal of the Royal Statistical Society: Series B (Methodological), 55}(2),
493–508.

White, H. (1980). A heteroskedasticity-consistent covariance matrix estimator
and a direct test for heteroskedasticity. \emph{Econometrica, 48}(4), 817–838.
https://doi.org/10.2307/1912934
}
\seealso{
\code{\link{check.resid}}, \code{\link{test.linear}}, \code{\link{test.norm}}, \code{\link{coeff.robust}}
}
\author{
Takuya Yanagida
}
