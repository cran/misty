#' Residual Diagnostics
#'
#' This function performs residual diagnostics for linear models estimated by
#' using the \code{lm()} function and for multilevel and linear mixed-effects
#' models estimated by using the \code{lmer()} function from the \pkg{lme4}
#' package to detect nonlinearity (partial residual or component-plus-residual
#' plots), nonconstant error variance (predicted values vs. residuals plot), and
#' non-normality of residuals (Q-Q plot and histogram with density plot).
#'
#' @param model           a fitted model of class \code{"lm"}, \code{"lmerMod"}, or
#'                        \code{"lmerModLmerTest"}.
#' @param type            a character string specifying the type of the plot, i.e.,
#'                        \code{"linear"} for partial (component-plus-residual)
#'                        plots, \code{"homo"} (default) for predicted values vs.
#'                        residuals plot, and \code{"normal"} for Q-Q plot and
#'                        histogram with a density plot. Note that partial residual
#'                        or component-plus-residual plots are not available for
#'                        models with interaction terms
#'                        and for multilevel and linear mixed-effects models.
#' @param resid           a character string specifying the type of residual used
#'                        for the partial (component-plus-residual) plots or Q-Q plot
#'                        and histogram, i.e., \code{"unstand"} for unstandardized
#'                        residuals \code{"stand"} for standardized residuals, and
#'                        \code{"student"} for studentized residuals. By default,
#'                        studentized residuals are used for predicted values vs.
#'                        residuals plot and unstandardized residuals are used for
#'                        Q-Q plot and histogram. Note that studentized residuals
#'                        are not available for multilevel and linear mixed-effects
#'                        models when requesting Q-Q plots and histograms.
#' @param plot            logical: if \code{TRUE} (default), a plot is drawn.
#' @param point.shape     a numeric value for specifying the argument \code{shape}
#'                        in the \code{geom_point} function.
#' @param point.fill      a character string or numeric value for specifying the
#'                        argument \code{fill} in the \code{geom_point} function.
#' @param point.size      a numeric value for specifying the argument \code{size}
#'                        in the \code{geom_point} function.
#' @param line1           logical: if \code{TRUE} (default), regression line is drawn
#'                        in the partial (component-plus-residual) plots, horizontal
#'                        line is drawn in the predicted values vs. residuals plot,
#'                        and t-distribution or normal distribution curve is drawn
#'                        in the histogram.
#' @param line2           logical: if \code{TRUE} (default), Loess smooth line is
#'                        drawn in the partial (component-plus-residual) plots, loess
#'                        smooth lines are drawn in the predicted values vs. residuals
#'                        plot, and density curve is drawn in the histogram.
#' @param linetype1       a character string or numeric value for specifying the
#'                        argument \code{linetype} in the \code{geom_smooth},
#'                        \code{geom_hline}, or \code{stat_function} function.
#' @param linetype2       a character string or numeric value for specifying the
#'                        argument \code{linetype} in the \code{geom_smooth} or
#'                        \code{geom_density} function.
#' @param linewidth1      a numeric value for specifying the argument \code{linewidth}
#'                        in the \code{geom_smooth}, \code{geom_hline}, or
#'                        \code{stat_function} function.
#' @param linewidth2      a numeric value for specifying the argument \code{linewidth}
#'                        in the \code{geom_smooth} or \code{geom_density} function.
#' @param line.col1       a character string or numeric value for specifying the
#'                        argument \code{color} in the \code{geom_smooth},
#'                        \code{geom_hline}, or \code{stat_function} function.
#' @param line.col2       a character string or numeric value for specifying the
#'                        argument \code{color} in the \code{geom_smooth} or
#'                        \code{geom_density} function.
#' @param bar.width       a numeric value for specifying the argument \code{binwidth}
#'                        in the \code{geom_bar} function.
#' @param bar.n           a numeric value for specifying the argument \code{bins} in
#'                        the \code{geom_bar} function.
#' @param bar.col         a character string or numeric value for specifying the
#'                        argument \code{color} in the \code{geom_bar} function.
#' @param bar.fill        a character string or numeric value for specifying the
#'                        argument \code{fill} in the \code{geom_bar} function.
#' @param strip.text.size a numeric value for specifying the argument \code{size} in
#'                        the \code{element_text} function of the \code{strip.text}
#'                        argument within the \code{theme} function.
#' @param label.size      a numeric value for specifying the argument \code{size} in
#'                        the \code{element_text} function of the \code{axis.title}
#'                        argument within the \code{theme} function.
#' @param axis.text.size  a numeric value for specifying the argument \code{size} in
#'                        the \code{element_text} function of the \code{axis.text }
#'                        argument within the \code{theme} function.
#' @param xlim            a numeric vector with two elements for specifying the
#'                        argument \code{limits} in the \code{scale_x_continuous}
#'                        function.
#' @param ylim            a numeric vector with two elements for specifying the
#'                        argument \code{limits} in the \code{scale_y_continuous}
#'                        function.
#' @param xbreaks         a numeric vector for specifying the argument \code{breaks}
#'                        in the \code{scale_x_continuous} function.
#' @param ybreaks         a numeric vector for specifying the argument \code{breaks}
#'                        in the \code{scale_y_continuous} function.
#' @param check           logical: if \code{TRUE} (default), argument specification
#'                        is checked.
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
#' and draws the linear regression and the loess smooth line to the partial residual
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
#' residuals. It also draws a horizontal line at 0, a loess smooth lines for all
#' residuals as well as separate loess smooth lines for positive and negative residuals.}
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
#' \item{\code{plotdat}}{data frame used for the plot}
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
#' #----------------------------------------------------------------------------
#' # Linear Model
#'
#' # Estimate linear model
#' mod.lm <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
#'
#' # Example 1a: Partial (component-plus-residual) plots
#' check.resid(mod.lm, type = "linear")
#'
#' # Example 1b: Predicted values vs. residuals plot
#' check.resid(mod.lm, type = "homo")
#'
#' # Example 1c: Q-Q plot and histogram with density plot
#' check.resid(mod.lm, type = "normal")
#'
#' # Example 1d: Extract data and ggplot2 object
#' object <- check.resid(mod.lm, type = "linear", plot = FALSE)
#'
#' # Data frame
#' object$plotdat
#'
#' # ggplot object
#' object$plot
#'
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Multilevel and Linear Mixed-Effects Model
#'
#' # Load lme4 package
#' library(lme4)
#'
#' # Estimate two-level mixed-effects model
#' mod.lmer <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy)
#'
#' # Example 2a: Partial (component-plus-residual) plots
#' check.resid(mod.lmer, type = "linear")
#'
#' # Example 2b: Predicted values vs. residuals plot
#' check.resid(mod.lmer, type = "homo")
#'
#' # Example 2c: Q-Q plot and histogram with density plot
#' check.resid(mod.lmer, type = "normal")
#' }
check.resid <- function(model, type = c("linear", "homo", "normal"),
                        resid = c("unstand", "stand", "student"), plot = TRUE,
                        point.shape = 21, point.fill = "gray80", point.size = 1,
                        line1 = TRUE, line2 = TRUE, linetype1 = "solid", linetype2 = "dashed",
                        linewidth1 = 1, linewidth2 = 1, line.col1 = "#0072B2", line.col2 = "#D55E00",
                        bar.width = NULL, bar.n = 30, bar.col = "black", bar.fill = "gray95",
                        strip.text.size = 11, label.size = 10, axis.text.size = 10,
                        xlim = NULL, ylim = NULL, xbreaks = ggplot2::waiver(),
                        ybreaks = ggplot2::waiver(), check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing
  if (isTRUE(missing(model))) { stop("Input for the argument 'model' is missing.", call. = FALSE) }

  # Check if input 'model' is NULL
  if (isTRUE(is.null(model))) { stop("Input specified for the argument 'model' is NULL.", call. = FALSE) }

  # Check if input 'model' is not 'lm', 'lmerMod', or 'lmerModLmerTest'
  if (isTRUE(!any(class(model) %in% c("lm", "lmerMod", "lmerModLmerTest")))) { stop("Please specify an \"lm\", \"lmerMod\", or \"lmerModLmerTest\" object for the argument 'model'.", call. = FALSE) }

  # Check if input 'model' has an interaction term
  if (isTRUE(any(attr(terms(model), "order") > 1L) && all(type == "linear"))) { stop("Component-plus-residual plots are not available for models with interactions.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs and packages
  .check.input(logical = c("line1", "line2", "plot"),
               numeric = list(point.shape = 1L, point.size = 1L, linewidth1 = 1L, linewidth2 = 1L, bar.width = 1L, bar.n = 1L, strip.text.size = 1L, label.size = 1L, axis.text.size = 1L, xlim = 2L, ylim = 2L),
               s.character = list(type = c("linear", "homo", "normal"), resid = c("unstand", "stand", "student")),
               package = c("ggplot2", "patchwork"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type ####

  if (isTRUE(all(c("linear", "homo", "normal") %in% type))) { type <- "homo" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual ####

  if (isTRUE(all(c("unstand", "stand", "student") %in% resid))) { if (isTRUE(type != "normal")) { resid <- "student" } else { resid <- "unstand" } }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # White background
  ggplot2::theme_set(ggplot2::theme_bw())

  pred <- condval <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Linearity ####

  switch(type, linear = {

    # Check if model has at least one predictor
    if (isTRUE(if (isTRUE(class(model) == "lm")) { length(attr(model$terms, "term.labels")) == 0L } else { length(lme4::fixef(model) |> (\(y) y[names(y) != "(Intercept)"])()) == 0L})) { stop("There are no predictors in the model specified in the argument 'model'.", call. = FALSE) }

    #...................
    ### Linear Model ####

    if (isTRUE(class(model) == "lm")) {

      ##### Predictor variables ####

      pred.names <- attr(model$terms, "term.labels")

      ##### Data frame ####

      plotdat <- data.frame(var = factor(rep(pred.names, each = nrow(model$model)), levels = pred.names),
                            pred = unlist(model$model[, pred.names]),
                            resid = as.vector(residuals(model, type = "partial")),
                            row.names = NULL)

      ##### Plot data ####

      p <- ggplot2::ggplot(plotdat, ggplot2::aes(pred, resid)) +
            ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill) +
            ggplot2::facet_wrap(~ var, scales = "free_x") +
            ggplot2::scale_x_continuous("Predictor", limits = xlim, breaks = xbreaks) +
            ggplot2::scale_y_continuous("Partial Residual", limits = ylim, breaks = ybreaks) +
            ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                           axis.title = ggplot2::element_text(size = label.size),
                           axis.text = ggplot2::element_text(size = axis.text.size))

      # Add lines
      if (isTRUE(line1)) { p <- p +  ggplot2::geom_smooth(method = "lm", se = FALSE, formula = "y ~ x", linetype = linetype1, linewidth = linewidth1, color = line.col1) }

      if (isTRUE(line2)) { p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, formula = "y ~ x", linetype = linetype2, linewidth = linewidth2, color = line.col2) }

    #...................
    ### Multilevel and Linear Mixed-Effects Model, lmer() ####

    } else if (isTRUE(class(model) %in% c("lmerMod", "lmerModLmerTest"))) {

      ##### Data frame ####

      # Model Data
      model.data <- model.frame(model)

      # Names of the predictors
      pred.names <- names(lme4::fixef(model) |> (\(y) y[names(y) != "(Intercept)"])())

      # Data
      plotdat <- data.frame(var = factor(rep(pred.names, each = nrow(model.data)), levels = pred.names),
                             pred = unlist(model.data[, pred.names]),
                             resid = as.vector(sapply(pred.names, function(y) .resid.partial(model, fix = if (length(pred.names) != 1L) { misty::chr.omit(pred.names, omit = y) } else { NULL }, ran = "all"))),
                             row.names = NULL)

      ##### Plot data ####

      p <- ggplot2::ggplot(plotdat, ggplot2::aes(pred, resid)) +
        ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill) +
        ggplot2::facet_wrap(~ var, scales = "free_x") +
        ggplot2::scale_x_continuous("Predictor", limits = xlim, breaks = xbreaks) +
        ggplot2::scale_y_continuous("Partial Residual", limits = ylim, breaks = ybreaks) +
        ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                       axis.title = ggplot2::element_text(size = label.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

      # Add lines
      if (isTRUE(line1)) { p <- p +  ggplot2::geom_smooth(method = "lm", se = FALSE, formula = "y ~ x", linetype = linetype1, linewidth = linewidth1, color = line.col1) }

      if (isTRUE(line2)) { p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, formula = "y ~ x", linetype = linetype2, linewidth = linewidth2, color = line.col2) }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Homoscedasticity ####

  }, homo = {

    #...................
    ### Linear Model ####

    if (isTRUE(class(model) == "lm")) {

      ##### Residual ####

      # Unstandardized residuals
      switch(resid, unstand = {

        plotdat <- data.frame(pred = predict(model), resid = residuals(model))

      # Standardized residuals
      }, stand =  {

        plotdat <- data.frame(pred = predict(model), resid = rstandard(model))

      # Studentized residuals
      }, student = {

        plotdat <- data.frame(pred = predict(model), resid = rstudent(model))

      })

      ##### Limits and of the x and y-axis ####

      if (isTRUE(is.null(ylim))) { ylim <- c(-ceiling(max(abs(plotdat$resid))), ceiling(max(abs(plotdat$resid)))) }

      ##### Plot data ####

      p <- ggplot2::ggplot(plotdat, ggplot2::aes(pred, resid)) +
            ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill) +
            ggplot2::scale_x_continuous("Predicted Value", limits = xlim, breaks = xbreaks) +
            ggplot2::scale_y_continuous(switch(resid, unstand = "Unstandardized Residual", stand = "Standardized Residual", student = "Studentized Residual"),
                                        limits = ylim, breaks = ybreaks) +
            ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                           axis.title = ggplot2::element_text(size = label.size),
                           axis.text = ggplot2::element_text(size = axis.text.size))

      # Add lines
      if (isTRUE(line1)) { p <- p + ggplot2::geom_hline(yintercept = 0, linetype = linetype1, linewidth = linewidth1, color = line.col1) }

      if (isTRUE(line2)) { p <- p + ggplot2::geom_smooth(method = loess, se = FALSE, formula = "y ~ x", linetype = linetype2, linewidth = linewidth2, color = line.col2) +
                                      ggplot2::geom_smooth(data = subset(plotdat, resid > 0L), method = loess, formula = "y ~ x", se = FALSE, linetype = linetype2, linewidth = linewidth2, color = line.col2) +
                                      ggplot2::geom_smooth(data = subset(plotdat, resid < 0L), method = loess, formula = "y ~ x", se = FALSE, linetype = linetype2, linewidth = linewidth2, color = line.col2) }

    #...................
    ### Multilevel and Linear Mixed-Effects Model, lmer() ####

    } else if (isTRUE(class(model) %in% c("lmerMod", "lmerModLmerTest"))) {

      ##### Residuals ####

      # Unstandardized residuals
      switch(resid, unstand = {

        plotdat <- data.frame(pred = predict(model), resid = residuals(model))

      # Standardized residuals
      }, stand =  {

        plotdat <- data.frame(pred = predict(model), resid = resid(model, type = "pearson", scale = TRUE))

      # Studentized residuals
      }, student = {

        plotdat <- data.frame(pred = predict(model), resid = rstudent(model))

      })

      ##### Limits and of the x and y-axis ####

      if (isTRUE(is.null(ylim))) { ylim <- c(-ceiling(max(abs(plotdat$resid))), ceiling(max(abs(plotdat$resid)))) }

      ##### Plot data ####

      p <- ggplot2::ggplot(plotdat, ggplot2::aes(pred, resid)) +
        ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill) +
        ggplot2::scale_x_continuous("Predicted Value", limits = xlim, breaks = xbreaks) +
        ggplot2::scale_y_continuous(switch(resid, unstand = "Unstandardized Residual", stand = "Standardized Residual", student = "Studentized Residual"),
                                    limits = ylim, breaks = ybreaks) +
        ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                       axis.title = ggplot2::element_text(size = label.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

      # Add lines
      if (isTRUE(line1)) { p <- p + ggplot2::geom_hline(yintercept = 0, linetype = linetype1, linewidth = linewidth1, color = line.col1) }

      if (isTRUE(line2)) { p <- p + ggplot2::geom_smooth(method = loess, se = FALSE, formula = "y ~ x", linetype = linetype2, linewidth = linewidth2, color = line.col2) +
                                      ggplot2::geom_smooth(data = subset(plotdat, resid > 0L), method = loess, formula = "y ~ x", se = FALSE, linetype = linetype2, linewidth = linewidth2, color = line.col2) +
                                      ggplot2::geom_smooth(data = subset(plotdat, resid < 0L), method = loess, formula = "y ~ x", se = FALSE, linetype = linetype2, linewidth = linewidth2, color = line.col2) }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Normality of Residuals ####

  }, normal =  {

    #...................
    ### Linear Model ####

    if (isTRUE(class(model) == "lm")) {

      ##### Residuals ####

      # Unstandardized residuals
      switch(resid, unstand = {

        plotdat <- data.frame(resid = residuals(model))

      # Standardized residuals
      }, stand =  {

        plotdat <- data.frame(resid = rstandard(model))

      # Studentized residuals
      }, student = {

        plotdat <- data.frame(resid = rstudent(model))

      })

      #### Q-Q Plot ####

      p1 <- ggplot2::ggplot(plotdat, ggplot2::aes(sample = resid)) +
              ggplot2::stat_qq(size = point.size, shape = point.shape, fill = point.fill) +
              ggplot2::scale_x_continuous(if (isTRUE(resid == "student")) { "Theoretical Quantiles of the t Distribution"  } else { "Theoretical Quantiles of the Normal Distribution" },
                                          limits = xlim, breaks = xbreaks) +
              ggplot2::scale_y_continuous(switch(resid, unstand = "Sample Quantile of Unstandardized Residual", stand = "Sample Quantile of Standardized Residual",
                                                 student = "Sample Quantile of Studentized Residual"),
                                          limits = ylim, breaks = ybreaks) +
              ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                             axis.title = ggplot2::element_text(size = label.size),
                             axis.text = ggplot2::element_text(size = axis.text.size))

      # Add line
      if (isTRUE(resid == "student")) {

        p1 <- p1 + ggplot2::stat_qq_line(distribution = qt, dparams = length(plotdat$resid) - length(attr(model$terms, "term.labels")) - 2L)

      } else {

        p1 <- p1 + ggplot2::stat_qq_line()

      }

      #### Histogram ####

      # Limits of the x-axis
      if (isTRUE(is.null(xlim))) { xlim <- c(-ceiling(max(abs(plotdat$resid))), ceiling(max(abs(plotdat$resid)))) }

      # Plot data
      p2 <- ggplot2::ggplot(plotdat, ggplot2::aes(resid)) +
              ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                                   binwidth = bar.width, bins = bar.n, color = bar.col, fill = bar.fill) +
              ggplot2::scale_x_continuous(switch(resid, unstand = "Unstandardized Residual", stand = "Standandardized Residual", student = "Studentized Residual"),
                                          limits = xlim, breaks = xbreaks) +
              ggplot2::scale_y_continuous("Density", limits = ylim, breaks = ybreaks) +
              ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                             axis.title = ggplot2::element_text(size = label.size),
                             axis.text = ggplot2::element_text(size = axis.text.size))

      # Add lines
      if (isTRUE(line1)) { p2 <- p2 + ggplot2::stat_function(fun = if (isTRUE(resid == "student")) { dt } else { dnorm },
                                                             args = if (isTRUE(resid == "student")) { list(df = length(na.omit(plotdat$resid)) - length(attr(model$terms, "term.labels")) - 2L) } else { list(mean = mean(plotdat$resid, na.rm = TRUE), sd = sd(plotdat$resid, na.rm = TRUE)) },
                                                             col = line.col1, linetype = linetype1, linewidth = linewidth1) }

      if (isTRUE(line2)) { p2 <- p2 + ggplot2::geom_density(color = line.col2, linetype = linetype2, linewidth = linewidth2) }

      #### Arrange plots ####

      p <- patchwork::wrap_plots(p1, p2, ncol = 2L)

    #...................
    ### Multilevel and Linear Mixed-Effects Model, lmer() ####

    } else if (isTRUE(class(model) %in% c("lmerMod", "lmerModLmerTest"))) {

      ##### Level-1 Residuals ####

      if (isTRUE(resid == "student")) { stop("Studentized residuals are not available for multilevel and linear mixed-effects model.", call. = FALSE) }

      # Unstandardized residuals
      switch(resid, unstand = {

        plotdat1 <- data.frame(resid = residuals(model))

      # Standardized residuals
      }, stand =  {

        plotdat1 <- data.frame(resid = resid(model, type = "pearson", scale = TRUE))

      })

      ##### Level-2 Residuals ####

      # Unstandardized or standardized residuals
      plotdat2 <- as.data.frame(lme4::ranef(model)) |>
        (\(y) merge(y, sqrt(diag(VarCorr(model)[[1L]])) |> (\(z) data.frame(term = names(z), sd = z, row.names = NULL))(), by = "term"))() |>
        (\(w) if (isTRUE(resid == "stand")) { within(w, assign("condval", condval / sd)) } else { w })()

      # Level-1 and Level-2 Residuals
      plotdat <- list(plotdat1, plotdat2)

      #### Level-1 Q-Q Plot ####

      p11 <- ggplot2::ggplot(plotdat1, ggplot2::aes(sample = resid)) +
        ggplot2::stat_qq(size = point.size, shape = point.shape, fill = point.fill) +
        ggplot2::scale_x_continuous("Theoretical Quantiles of the Normal Distribution", limits = xlim, breaks = xbreaks) +
        ggplot2::stat_qq_line() +
        ggplot2::scale_y_continuous(switch(resid, unstand = "Sample Quantile of Unstandardized Residual", stand = "Sample Quantile of Standardized Residual"),
                                    limits = ylim, breaks = ybreaks) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = -0.05, vjust = 2.5),
                       strip.text = ggplot2::element_text(size = strip.text.size),
                       axis.title = ggplot2::element_text(size = label.size),
                       axis.text = ggplot2::element_text(size = axis.text.size),
                       plot.margin = ggplot2::margin(5.5, 5.5, 20, 5.5, "pt")) +
        ggplot2::ggtitle("Level-1 Residuals")

      #### Level-2 Q-Q Plot ####

      p21 <- ggplot2::ggplot(plotdat2, ggplot2::aes(sample = condval)) +
        ggplot2::stat_qq(size = point.size, shape = point.shape, fill = point.fill) +
        ggplot2::scale_x_continuous("Theoretical Quantiles of the Normal Distribution", limits = xlim, breaks = xbreaks) +
        ggplot2::stat_qq_line() +
        ggplot2::facet_wrap(~ term, nrow = misty::uniq.n(plotdat2$term), scales = "free_y") +
        ggplot2::scale_y_continuous(switch(resid, unstand = "Sample Quantile of Unstandardized Residual", stand = "Sample Quantile of Standardized Residual"),
                                    limits = ylim, breaks = ybreaks) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = -0.05, vjust = 2.5),
                       strip.text = ggplot2::element_text(size = strip.text.size),
                       axis.title = ggplot2::element_text(size = label.size),
                       axis.text = ggplot2::element_text(size = axis.text.size)) +
        ggplot2::ggtitle("Level-2 Residuals")

      #### Level-1 Histogram ####

      # Limits of the x-axis
      if (isTRUE(is.null(xlim))) { xlim.d <- c(-ceiling(max(abs(plotdat1$resid))), ceiling(max(abs(plotdat1$resid)))) }

      # Plot data
      p12 <- ggplot2::ggplot(plotdat1, ggplot2::aes(resid)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                binwidth = bar.width, bins = bar.n, color = bar.col, fill = bar.fill) +
        ggplot2::scale_x_continuous(switch(resid, unstand = "Unstandardized Residual", stand = "Standandardized Residual"),
                                    limits = if (isTRUE(is.null(xlim))) { xlim.d } else { xlim }, breaks = xbreaks) +
        ggplot2::scale_y_continuous("Density", limits = ylim, breaks = ybreaks) +
        ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                       axis.title = ggplot2::element_text(size = label.size),
                       axis.text = ggplot2::element_text(size = axis.text.size),
                       plot.margin = ggplot2::margin(5.5, 5.5, 20, 5.5, "pt"))

      # Add lines
      if (isTRUE(line1)) { p12 <- p12 + ggplot2::stat_function(fun = dnorm , args = list(mean = mean(plotdat1$resid, na.rm = TRUE), sd = sd(plotdat1$resid, na.rm = TRUE)), col = line.col1, linetype = linetype1, linewidth = linewidth1) }

      if (isTRUE(line2)) { p12 <- p12 + ggplot2::geom_density(color = line.col2, linetype = linetype2, linewidth = linewidth2) }

      #### Level-2 Histogram ####

      # Limits of the x-axis
      if (isTRUE(is.null(xlim))) { xlim.d <- c(-ceiling(max(abs(plotdat2$condval))) - 0.1, ceiling(max(abs(plotdat2$condval))) + 0.1) }

      # Plot data
      p22 <- ggplot2::ggplot(plotdat2, ggplot2::aes(condval)) +
        ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)), binwidth = bar.width, bins = bar.n, color = bar.col, fill = bar.fill) +
        ggplot2::scale_x_continuous(switch(resid, unstand = "Unstandardized Residual", stand = "Standandardized Residual"),
                                    limits = if (isTRUE(is.null(xlim))) { xlim.d } else { xlim }, breaks = xbreaks) +
        ggplot2::facet_wrap(~ term, nrow = misty::uniq.n(plotdat2$term), scales = "free_y") +
        ggplot2::scale_y_continuous("Density", limits = ylim, breaks = ybreaks) +
        ggplot2::theme(strip.text = ggplot2::element_text(size = strip.text.size),
                       axis.title = ggplot2::element_text(size = label.size),
                       axis.text = ggplot2::element_text(size = axis.text.size))

      # Add lines
      if (isTRUE(line1)) { p22 <- p22 + ggplot2::stat_function(fun = dnorm , args = list(mean = mean(plotdat2$condval, na.rm = TRUE), sd = sd(plotdat2$condval, na.rm = TRUE)), col = line.col1, linetype = linetype1, linewidth = linewidth1) }

      if (isTRUE(line2)) { p22 <- p22 + ggplot2::geom_density(color = line.col2, linetype = linetype2, linewidth = linewidth2) }

      #### Arrange plots ####

      p <- patchwork::wrap_plots(patchwork::wrap_plots(p11, p12, ncol = 2), patchwork::wrap_plots(p21, p22, ncol = 2), nrow = 2) +
            patchwork::plot_layout(heights = c(1L, misty::uniq.n(plotdat2$term)))

    }

  })

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "check.resid",
                 model = model,
                 args = list(type = type, resid = resid,
                             point.shape = point.shape, point.fill = point.fill, point.size = point.size,
                             line1 = line1, line2 = line2, linetype1 = linetype1, linetype2 = linetype2,
                             linewidth1 = linewidth1, linewidth2 = linewidth2, line.col1 = line.col1, line.col2 = line.col2,
                             bar.width = bar.width, bar.n = bar.n, bar.col = bar.col, bar.fill = bar.fill,
                             strip.text.size = strip.text.size, label.size = label.size, axis.text.size = axis.text.size,
                             xlim = xlim, ylim = ylim, xbreaks = xbreaks, ybreaks = ybreaks,
                             check = check, plot = plot),
                 plotdat = plotdat, plot = p)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot -----------------------------------------------------------------------

  if (isTRUE(plot)) { invisible(suppressMessages(suppressWarnings(print(p)))) }

  return(invisible(object))

}

#_______________________________________________________________________________
