#' Residual Diagnostics for Linear, Multilevel and Mixed-Effects Models
#'
#' This function performs residual diagnostics for linear models estimated by
#' using the \code{lm()} function and for multilevel and linear mixed-effects
#' models estimated by using the \code{lmer()} function from the \pkg{lme4}
#' package to detect nonlinearity (partial residual or component-plus-residual
#' plots), nonconstant error variance (predicted values vs. residuals plot), and
#' non-normality of residuals (Q-Q plot and histogram with density curves).
#'
#' @param model    a fitted model of class \code{"lm"}, \code{"lmerMod"}, or
#'                 \code{"lmerModLmerTest"}.
#' @param type     a character string specifying the type of the plot, i.e.,
#'                 \code{"linear"} for partial (component-plus-residual)
#'                 plots, \code{"homo"} (default) for predicted values vs.
#'                 residuals plot, and \code{"normal"} for Q-Q plot and
#'                 histogram with a density plot. Note that partial residual
#'                 or component-plus-residual plots are not available for
#'                 models with interaction terms
#'                 and for multilevel and linear mixed-effects models.
#' @param resid    a character string specifying the type of residual used
#'                 for the partial (component-plus-residual) plots or Q-Q plot
#'                 and histogram, i.e., \code{"unstand"} for unstandardized
#'                 residuals \code{"stand"} for standardized residuals, and
#'                 \code{"student"} for studentized residuals. By default,
#'                 studentized residuals are used for predicted values vs.
#'                 residuals plot and unstandardized residuals are used for
#'                 Q-Q plot and histogram. Note that studentized residuals
#'                 are not available for multilevel and linear mixed-effects
#'                 models when requesting Q-Q plots and histograms.
#' @param plot     logical: if \code{TRUE} (default), a plot is drawn.
#' @param geom     a character vector indicating which plot elements to draw, i.e.,
#'                 \code{"all"} for drawing all plot elements, \code{"none"} for
#'                 drawing no regression or smoothing curve, \code{"lm"} for drawing
#'                 the regression line, \code{"loess"} for drawing the LOESS smoothing
#'                 curve, \code{"ci.lm"} for drawing confidence bands around the
#'                 regression line, \code{"ci.loess"} for drawing confidence bands
#'                 around the LOESS smoothing curve, \code{"hline"} or drawing a
#'                 horizontal line at \code{0}, \code{"qq"} for plotting Q-Q plots,
#'                 and \code{"hist"} for plotting histograms. Note that the arguments
#'                 \code{"lm"}, \code{"loess"}, \code{"ci.lm"}, and \code{"ci.less"}
#'                 are applicable when \code{type = "linear"}, the arguments
#'                 \code{"hline"}, \code{"loess"}, and \code{"ci.loess"} are
#'                 applicable when \code{type = "homo"}, and the arguments
#'                 \code{"lqq"} and \code{"hist"} are applicable when
#'                 \code{type = "norm"}.
#' @param filename a character string indicating the \code{filename}
#'                 argument including the file extension in the \code{ggsave}
#'                 function. Note that one of \code{".eps"}, \code{".ps"},
#'                 \code{".tex"}, \code{".pdf"} (default),
#'                 \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                 \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                 to be specified as file extension in the \code{file}
#'                 argument. Note that plots can only be saved when
#'                 \code{plot = TRUE}.
#' @param width    a numeric value indicating the \code{width} argument
#'                 (default is the size of the current graphics device)
#'                 for the \code{ggsave} function.
#' @param height   a numeric value indicating the \code{height} argument
#'                 (default is the size of the current graphics device)
#'                 for the \code{ggsave} function.
#' @param dpi      a numeric value indicating the \code{dpi} argument
#'                 (default is \code{600}) for the \code{ggsave} function.
#' @param check    logical: if \code{TRUE} (default), argument specification
#'                 is checked.
#'
#' @details
#' \describe{
#' \item{\strong{Nonlinearity}}{The violation of the assumption of linearity
#' implies that the model cannot accurately capture the systematic pattern of the
#' relationship between the outcome and predictor variables. In other words, the
#' specified regression surface does not accurately represent the relationship
#' between the conditional mean values of \eqn{Y} and the \eqn{X}s. That means
#' the average error \eqn{E(\varepsilon)} is not 0 at every point on the regression
#' surface (Fox, 2015).
#' In multiple regression, plotting the outcome variable \eqn{Y} against each
#' predictor variable \eqn{X} can be misleading because it does not reflect the
#' partial relationship between \eqn{Y} and \eqn{X} (i.e., statistically controlling
#' for the other \eqn{X}s), but rather the marginal relationship between \eqn{Y}
#' and \eqn{X} (i.e., ignoring the other \eqn{X}s). Partial residual plots or
#' component-plus-residual plots should be used to detect nonlinearity in
#' multiple regression. The partial residual for the \eqn{j}th predictor variable
#' is defined as
#'
#' \deqn{e_i^{(j)} = b_jX_{ij} + e_i}
#'
#' The linear component of the partial relationship between \eqn{Y} and \eqn{X_j}
#' is added back to the least-squares residuals, which may include an unmodeled
#' nonlinear component. Then, the partial residual \eqn{e_i^{(j)}} is plotted
#' against the predictor variable \eqn{X_j}. Nonlinearity may become apparent when
#' a non-parametric regression smoother is applied.
#' By default, the function plots each predictor against the partial residuals,
#' and draws the linear regression and the LOESS smoothing curve to the partial residual
#' plots.}
#'
#' \item{\strong{Nonconstant Error Variance}}{The violation of the assumption of
#' constant error variance, often referred to as heteroscedasticity, implies that
#' the variance of the outcome variable around the regression surface is not the
#' same at every point on the regression surface (Fox, 2015).
#' Plotting residuals against the outcome variable \eqn{Y} instead of the
#' predicted values \eqn{\hat{Y}} is not recommended because \eqn{Y = \hat{Y} + e}.
#' Consequently, the linear correlation between the outcome variable \eqn{Y} and
#' the residuals \eqn{e} is \eqn{\sqrt{1 - R^2}} where \eqn{R} is the multiple
#' correlation coefficient. In contrast, plotting residuals against the predicted
#' values \eqn{\hat{Y}} is much easier to examine for evidence of nonconstant
#' error variance as the correlation between \eqn{\hat{Y}} and \eqn{e} is 0. Note
#' that the least-squares residuals generally have unequal variance
#' \eqn{Var(e_i) = \sigma^2 / (1 - h_i)} where \eqn{h} is the leverage of
#' observation \eqn{i}, even if errors have constant variance \eqn{\sigma^2}.
#' The studentized residuals \eqn{e^*_i}, however, have a constant variance under
#' the assumption of the regression model. Residuals are studentized by dividing
#' them by \eqn{\sigma^2_i(\sqrt{(1 - h_i)}} where \eqn{\sigma^2_i} is the estimate
#' of \eqn{\sigma^2} obtained after deleting the \eqn{i}th observation, and \eqn{h_i}
#' is the leverage of observation \eqn{i} (Meuleman et al, 2015).
#' By default, the function plots the predicted values against the studentized
#' residuals. It also draws a horizontal line at 0, a LOESS smoothing curves for all
#' residuals as well as separate LOESS smoothing curves for positive and negative residuals.}
#'
#' \item{\strong{Non-normality of Residuals}}{Statistical inference under the
#' violation of the assumption of normally distributed errors is approximately
#' valid in all but small samples. However, the efficiency of least squares is
#' not robust because the least-squares estimator is the most efficient and
#' unbiased estimator only when the errors are normally distributed. For instance,
#' when error distributions have heavy tails, the least-squares estimator becomes
#' much less efficient compared to robust estimators. In addition, error
#' distributions with heavy-tails result in outliers and compromise the
#' interpretation of conditional means because the mean is not an accurate measure
#' of central tendency in a highly skewed distribution. Moreover, a multimodal
#' error distribution suggests the omission of one or more discrete explanatory
#' variables that naturally divide the data into groups (Fox, 2016).
#'
#' By default, the function plots a Q-Q plot of the unstandardized residuals, and
#' a histogram of the unstandardized residuals and a density plot. Note that
#' studentized residuals follow a \eqn{t}-distribution with \eqn{n - k - 2} degrees
#' of freedom where \eqn{n} is the sample size and \eqn{k} is the number of
#' predictors. However, the normal and \eqn{t}-distribution are nearly identical
#' unless the sample size is small. Moreover, even if the model is correct, the
#' studentized residuals are not an independent random sample from \eqn{t_{n - k - 2}}.
#' Residuals are correlated with each other depending on the configuration of the
#' predictor values. The correlation is generally negligible unless the sample
#' size is small.}
#' }
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{check.collin}}, \code{\link{check.outlier}}
#'
#' @references
#' Fox, J. (2016). \emph{Applied regression analysis and generalized linear models}
#' (3rd ed.). Sage Publications, Inc.
#'
#' Hohenstein, S., & Kliegl, R. (2025). \emph{remef: Remove Partial Effects}.
#' R package version 1.0.7, \url{https://github.com/hohenstein/remef}
#'
#' Meuleman, B., Loosveldt, G., & Emonds, V. (2015). Regression analysis: Assumptions
#' and diagnostics. In H. Best & C. Wolf (Eds.), \emph{The SAGE handbook of regression
#' analysis and causal inference (pp. 83-110)}. Sage.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{model}}{model specified in \code{model }}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{plot}}{ggplot2 object for plotting the residuals}
#'
#' @note
#' This function uses a modified copy of the \code{partial()} and \code{calc_ranef()}
#' function in the \pkg{remef} package by Sven Hohenstein and Reinhold Kliegl (2025)
#' when requesting partial residual plots for linear mixed-effects models.
#'
#' @export
#'
#' @examples
#' #————————————————————————————————————————————————————————————————————————————
#' # Linear Model
#'
#' # Estimate linear model
#' mod.lm <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
#'
#' #——————————————————————————————————————
#' ## Nonlinearity
#'
#' # Example 1a: Partial (component-plus-residual) plots
#' check.resid(mod.lm, type = "linear")
#'
#' # Example 1b: Plot with smoothing curve only
#' check.resid(mod.lm, type = "linear", geom = "loess")
#'
#' # Example 1c: Additional arguments
#' # see Details in the help page of the function plot.misty.object
#' check.resid(mod.lm, type = "linear", point.size = 1, point.shape = 2)
#'
#' #——————————————————————————————————————
#' ## Nonconstant Error Variance
#'
#' # Example 2a: Predicted values vs. residuals plot
#' check.resid(mod.lm, type = "homo")
#'
#' # Example 2b: Plot with horizontal line only
#' check.resid(mod.lm, type = "homo", geom = "hline")
#'
#' # Example 2c: Additional arguments
#' # see Details in the help page of the function plot.misty.object
#' check.resid(mod.lm, type = "homo", point.shape = 2, linetype = "solid", line.col = "blue")
#'
#' #——————————————————————————————————————
#' ## Non-Normality of Residuals
#'
#' # Example 3a: Q-Q plot and histogram with density plot
#' check.resid(mod.lm, type = "normal")
#'
#' # Example 3b: Q-Q plot only
#' check.resid(mod.lm, type = "normal", geom = "qq")
#'
#' # Example 2c: Additional arguments
#' # see Details in the help page of the function plot.misty.object
#' check.resid(mod.lm, type = "normal", point.shape = 2, abline.col = "red")
#'
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Multilevel and Linear Mixed-Effects Model
#'
#' # Load lme4 package
#' library(lme4)
#'
#' # Estimate two-level mixed-effects model
#' mod.lmer <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'
#' #——————————————————————————————————————
#' ## Nonlinearity
#'
#' # Example 4a: Partial (component-plus-residual) plots
#' check.resid(mod.lmer, type = "linear")
#'
#' # Example 4b: Plot with smoothing curve only
#' check.resid(mod.lmer, type = "linear", geom = "loess")
#'
#' # Example 4c: Additional arguments
#' # see Details in the help page of the function plot.misty.object
#' check.resid(mod.lmer, type = "linear", point.size = 1, point.shape = 2)
#'
#' #——————————————————————————————————————
#' ## Nonconstant Error Variance
#'
#' # Example 5a: Predicted values vs. residuals plot
#' check.resid(mod.lmer, type = "homo")
#'
#' # Example 5b: Plot with horizontal line only
#' check.resid(mod.lmer, type = "homo", geom = "hline")
#'
#' # Example 5c: Additional arguments
#' # see Details in the help page of the function plot.misty.object
#' check.resid(mod.lmer, type = "homo", point.shape = 2, linetype = "solid",
#'             line.col = "blue")
#'
#' #——————————————————————————————————————
#' ## Non-Normality of Residuals
#'
#' # Example 6c: Q-Q plot and histogram with density plot
#' check.resid(mod.lmer, type = "normal")
#'
#' # Example 6b: Q-Q plot only
#' check.resid(mod.lmer, type = "normal", geom = "qq")
#'
#' # Example 6c: Additional arguments
#' # see Details in the help page of the function plot.misty.object
#' check.resid(mod.lmer, type = "normal", point.shape = 2, abline.col = "red")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Save Plot
#'
#' # Example 7: Save plot as PNG fine
#' check.resid(mod.lm, type = "linear", filename = "Linear.png", width = 15, height = 5)
#' }
check.resid <- function(model, type = c("linear", "homo", "normal"),
                        resid = c("unstand", "stand", "student"), plot = TRUE,
                        geom = c("all", "none", "lm", "loess", "ci.lm", "ci.loess", "hline", "qq", "hist"),
                        filename = NULL, width = NA, height = NA, dpi = 600, check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check ——————————————————————————————————————————————————————————————

  # Check if input 'model' is missing or NULL
  if (isTRUE(missing(model) || is.null(model))) { stop("Please specify a fitted model for the argument 'model'.", call. = FALSE) }

  # Check if input 'model' is not 'lm', 'lmerMod', or 'lmerModLmerTest'
  if (isTRUE(!any(class(model) %in% c("lm", "lmerMod", "lmerModLmerTest")))) { stop("Please specify an \"lm\", \"lmerMod\", or \"lmerModLmerTest\" object for the argument 'model'.", call. = FALSE) }

  # Check if 'model' has at least one predictor
  if (isTRUE(if (isTRUE(class(model) == "lm")) { length(attr(model$terms, "term.labels")) == 0L } else { length(lme4::fixef(model) |> (\(y) y[names(y) != "(Intercept)"])()) == 0L})) { stop("There are no predictors in the model specified in the argument 'model'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ————————————————————————————————————————————————————————————————

  .check.input(logical = "plot", s.character = list(type = c("linear", "homo", "normal"), resid = c("unstand", "stand", "student")), m.character = list(geom = c("all", "none", "lm", "loess", "ci.lm", "ci.loess", "hline", "qq", "hist")), package = c("ggplot2", "patchwork"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Arguments ——————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type ####

  if (isTRUE(all(c("linear", "homo", "normal") %in% type))) { type <- "homo" }

  # Check if 'model' has an interaction term
  if (isTRUE(type == "linear")) { if (isTRUE(any(attr(terms(model), "order") > 1L))) { stop("Component-plus-residual plots are not available for models with interactions.", call. = FALSE) } }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual ####

  if (isTRUE(all(c("unstand", "stand", "student") %in% resid))) { if (isTRUE(type != "normal")) { resid <- "student" } else { resid <- "unstand" } }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'geom' Argument ####

  switch(type, "linear" = {

    if (isTRUE(all(c("all", "none", "lm", "loess", "ci.lm", "ci.loess", "hline", "qq", "hist") %in% geom) || all(geom == "all"))) { geom <- c("lm", "loess", "ci.lm", "ci.loess") }

  }, "homo" = {

    if (isTRUE(all(c("all", "none", "lm", "loess", "ci.lm", "ci.loess", "hline", "qq", "hist") %in% geom) || all(geom == "all"))) { geom <- c("hline", "loess", "ci.loess") }

  }, "normal" = {

    if (isTRUE(all(c("all", "none", "lm", "loess", "ci.lm", "ci.loess", "hline", "qq", "hist") %in% geom) || all(geom == "all"))) { geom <- c("qq", "hist") }


  })

  #_____________________________________________________________________________
  #
  # Return object ——————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "check.resid",
                 model = model,
                 args = list(type = type, resid = resid, plot = plot, geom = geom, filename = filename, width = width, height = height, dpi = dpi, check = check),
                 plot = NULL)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  ## Plot and Save Plot ————————————————————————————————————————————————————————

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE, ...) |> (\(p) suppressMessages(suppressWarnings(print(p))))() }

  #_____________________________________________________________________________
  #
  # Output —————————————————————————————————————————————————————————————————————

  return(invisible(object))

}

#_______________________________________________________________________________
