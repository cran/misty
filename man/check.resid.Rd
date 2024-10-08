% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.resid.R
\name{check.resid}
\alias{check.resid}
\title{Residual Diagnostics}
\usage{
check.resid(model, type = c("linear", "homo", "normal"),
            resid = c("unstand", "stand", "student"),
            point.shape = 21, point.fill = "gray80", point.size = 1,
            line1 = TRUE, line2 = TRUE,
            line.type1 = "solid", line.type2 = "dashed",
            line.width1 = 1, line.width2 = 1,
            line.color1 = "#0072B2", line.color2 = "#D55E00",
            bar.width = NULL, bar.n = 30, bar.color = "black",
            bar.fill = "gray95", strip.size = 11,
            label.size = 10, axis.size = 10,
            xlimits = NULL, ylimits = NULL,
            xbreaks = ggplot2::waiver(), ybreaks = ggplot2::waiver(),
            check = TRUE, plot = TRUE)
}
\arguments{
\item{model}{a fitted model of class \code{lm}.}

\item{type}{a character string specifying the type of the plot, i.e.,
\code{"linear"} for partial (component-plus-residual) plots,
\code{"homo"} (default) for predicted values vs. residuals
plot, and \code{"normal"} for Q-Q plot and histogram with
a density plot. Note that partial plots are not available
for models with interaction terms.}

\item{resid}{a character string specifying the type of residual used for
the partial (component-plus-residual) plots or Q-Q plot and
histogram, i.e., \code{"unstand"} for unstandardized residuals
\code{"stand"} for standardized residuals, and \code{"student"}
for studentized residual. By default, studentized residuals
are used for predicted values vs. residuals plot and unstandardized
residuals are used for Q-Q plot and histogram.}

\item{point.shape}{a numeric value for specifying the argument \code{shape}
in the \code{geom_point} function.}

\item{point.fill}{a numeric value for specifying the argument \code{fill}
in the \code{geom_point} function.}

\item{point.size}{a numeric value for specifying the argument \code{size}
in the \code{geom_point} function.}

\item{line1}{logical: if \code{TRUE} (default), regression line is drawn
in the partial (component-plus-residual) plots, horizontal
line is drawn in the predicted values vs. residuals plot,
and t-distribution or normal distribution curve is drawn in
the histogram.}

\item{line2}{logical: if \code{TRUE} (default), Loess smooth line is drawn
in the partial (component-plus-residual) plots, loess smooth
lines are drawn in the predicted values vs. residuals plot,
and density curve is drawn in the histogram.}

\item{line.type1}{a character string or numeric value for specifying the argument
\code{linetype} in the \code{geom_smooth}, \code{geom_hline},
or \code{stat_function} function.}

\item{line.type2}{a character string or numeric value for specifying the argument
\code{linetype} in the \code{geom_smooth} or \code{geom_density}
 function.}

\item{line.width1}{a numeric value for specifying the argument \code{linewidth}
in the \code{geom_smooth}, \code{geom_hline}, or \code{stat_function}
function.}

\item{line.width2}{a numeric value for specifying the argument \code{linewidth}
in the \code{geom_smooth} or \code{geom_density} function.}

\item{line.color1}{a character string or numeric value for specifying the argument
\code{color} in the \code{geom_smooth}, \code{geom_hline},
or \code{stat_function} function.}

\item{line.color2}{a character string or numeric value for specifying the argument
\code{color} in the \code{geom_smooth} or \code{geom_density}
 function.}

\item{bar.width}{a numeric value for specifying the argument \code{bins} in
the \code{geom_bar} function.}

\item{bar.n}{a numeric value for specifying the argument \code{bins} in
the \code{geom_bar} function.}

\item{bar.color}{a character string or numeric value for specifying the argument
\code{color} in the \code{geom_bar} function.}

\item{bar.fill}{a character string or numeric value for specifying the argument
\code{fill} in the \code{geom_bar} function.}

\item{strip.size}{a numeric value for specifying the argument \code{size} in
the \code{element_text} function of the \code{strip.text}
argument within the \code{theme} function.}

\item{label.size}{a numeric value for specifying the argument \code{size} in
the \code{element_text} function of the \code{axis.title}
argument within the \code{theme} function.}

\item{axis.size}{a numeric value for specifying the argument \code{size} in
the \code{element_text} function of the \code{axis.text }
argument within the \code{theme} function.}

\item{xlimits}{a numeric value for specifying the argument \code{limits}
in the \code{scale_x_continuous} function.}

\item{ylimits}{a numeric value for specifying the argument \code{limits}
in the \code{scale_y_continuous} function.}

\item{xbreaks}{a numeric value for specifying the argument \code{breaks }
in the \code{scale_x_continuous} function.}

\item{ybreaks}{a numeric value for specifying the argument \code{breaks }
in the \code{scale_y_continuous} function.}

\item{check}{logical: if \code{TRUE} (default), argument specification is checked.}

\item{plot}{logical: if \code{TRUE} (default), a plot is drawn.}
}
\value{
Returns an object of class \code{misty.object}, which is a list with following
entries:
\item{\code{call}}{function call}
\item{\code{type}}{type of analysis}
\item{\code{model}}{model specified in \code{model}}
\item{\code{plotdat}}{data frame used for the plot}
\item{\code{args}}{specification of function arguments}
\item{\code{plot}}{ggplot2 object for plotting the residuals}
}
\description{
This function performs residual diagnostics for linear models estimated by
using the \code{lm()} function for detecting nonlinearity (partial residual or
component-plus-residual plots), nonconstant error variance (predicted values
vs. residuals plot), and non-normality of residuals (Q-Q plot and histogram
with density plot).
}
\details{
\describe{
\item{\strong{Nonlinearity}}{The violation of the assumption of linearity
implies that the model cannot accurately capture the systematic pattern of the
relationship between the outcome and predictor variables. In other words, the
specified regression surface does not accurately represent the relationship
between the conditional mean values of \eqn{Y} and the \eqn{X}s. That means
the average error \eqn{E(\varepsilon)} is not 0 at every point on the regression
surface (Fox, 2015).

In multiple regression, plotting the outcome variable \eqn{Y} against each predictor
variable \eqn{X} can be misleading because it does not reflect the partial
relationship between \eqn{Y} and \eqn{X} (i.e., statistically controlling for
the other \eqn{X}s), but rather the marginal relationship between \eqn{Y} and
\eqn{X} (i.e., ignoring the other \eqn{X}s). Partial residual plots or
component-plus-residual plots should be used to detect nonlinearity in multiple
regression. The partial residual for the \eqn{j}th predictor variable is defined
as

\deqn{e_i^{(j)} = b_jX_{ij} + e_i}

The linear component of the partial relationship between \eqn{Y} and \eqn{X_j}
is added back to the least-squares residuals, which may include an unmodeled
nonlinear component. Then, the partial residual \eqn{e_i^{(j)}} is plotted
against the predictor variable \eqn{X_j}. Nonlinearity may become apparent when
a non-parametric regression smoother is applied.

By default, the function plots
each predictor against the partial residuals, and draws the linear regression
and the loess smooth line to the partial residual plots.}

\item{\strong{Nonconstant Error Variance}}{The violation of the assumption of
constant error variance, often referred to as heteroscedasticity, implies that
the variance of the outcome variable around the regression surface is not the
same at every point on the regression surface (Fox, 2015).

Plotting residuals against the outcome variable \eqn{Y} instead of the predicted
values \eqn{\hat{Y}} is not recommended because \eqn{Y = \hat{Y} + e}. Consequently,
the linear correlation between the outcome variable \eqn{Y} and the residuals
\eqn{e} is \eqn{\sqrt{1 - R^2}} where \eqn{R} is the multiple correlation coefficient.
In contrast, plotting residuals against the predicted values \eqn{\hat{Y}} is
much easier to examine for evidence of nonconstant error variance as the correlation
between \eqn{\hat{Y}} and \eqn{e} is 0. Note that the least-squares residuals
generally have unequal variance \eqn{Var(e_i) = \sigma^2 / (1 - h_i)} where
\eqn{h} is the leverage of observation \eqn{i}, even if errors have constant
variance \eqn{\sigma^2}. The studentized residuals \eqn{e^*_i}, however, have
a constant variance under the assumption of the regression model. Residuals
are studentized by dividing them by \eqn{\sigma^2_i(\sqrt{(1 - h_i)}} where
\eqn{\sigma^2_i} is the estimate of \eqn{\sigma^2} obtained after deleting the
\eqn{i}th observation, and \eqn{h_i} is the leverage of observation \eqn{i}
(Meuleman et al, 2015).

By default, the function plots the predicted values
against the studentized residuals. It also draws a horizontal line at 0, a
loess smooth lines for all residuals as well as separate loess smooth lines
for positive and negative residuals.}

\item{\strong{Non-normality of Residuals}}{Statistical inference under the
violation of the assumption of normally distributed errors is approximately
valid in all but small samples. However, the efficiency of least squares is
not robust because the least-squares estimator is the most efficient and
unbiased estimator only when the errors are normally distributed. For instance,
when error distributions have heavy tails, the least-squares estimator becomes
much less efficient compared to robust estimators. In addition, error distributions with
heavy-tails result in outliers and compromise the interpretation of conditional
means because the mean is not an accurate measure of central tendency in a highly
skewed distribution. Moreover, a multimodal error distribution suggests the omission
of one or more discrete explanatory variables that naturally divide the data
into groups (Fox, 2016).

By default, the function plots a Q-Q plot of the unstandardized residuals, and
a histogram of the unstandardized residuals and a density plot. Note that
studentized residuals follow a \eqn{t}-distribution with \eqn{n - k - 2} degrees
of freedom where \eqn{n} is the sample size and \eqn{k} is the number of predictors.
However, the normal and \eqn{t}-distribution are nearly identical unless the
sample size is small. Moreover, even if the model is correct, the studentized
residuals are not an independent random sample from \eqn{t_{n - k - 2}}. Residuals
are correlated with each other depending on the configuration of the predictor
values. The correlation is generally negligible unless the sample size is small.}
}
}
\examples{
\dontrun{
#-------------------------------------------------------------------------------
# Residual diagnostics for a linear model

mod <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)

# Example 1: Partial (component-plus-residual) plots
check.resid(mod, type = "linear")

# Example 2: Predicted values vs. residuals plot
check.resid(mod, type = "homo")

# Example 3: Q-Q plot and histogram with density plot
check.resid(mod, type = "normal")

#-------------------------------------------------------------------------------
# Extract data and ggplot2 object

object <- check.resid(mod, type = "linear", plot = FALSE)

# Data frame
object$plotdat

# ggplot object
object$plot
}
}
\references{
Fox, J. (2016). \emph{Applied regression analysis and generalized linear models}
(3rd ed.). Sage Publications, Inc.

Meuleman, B., Loosveldt, G., & Emonds, V. (2015). Regression analysis: Assumptions
and diagnostics. In H. Best & C. Wolf (Eds.), \emph{The SAGE handbook of regression analysis and causal inference (pp. 83-110)}.
Sage.
}
\seealso{
\code{\link{check.collin}}, \code{\link{check.outlier}}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at}
}
