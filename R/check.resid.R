#' Residual Diagnostics
#'
#' This function performs residual diagnostics for linear models estimated by
#' using the \code{lm()} function for detecting nonlinearity (partial residual or
#' component-plus-residual plots), nonconstant error variance (predicted values
#' vs. residuals plot), and non-normality of residuals (Q-Q plot and histogram
#' with density plot).
#'
#' @param model       a fitted model of class \code{lm}.
#' @param type        a character string specifying the type of the plot, i.e.,
#'                    \code{"linear"} for partial (component-plus-residual) plots,
#'                    \code{"homo"} (default) for predicted values vs. residuals
#'                    plot, and \code{"normal"} for Q-Q plot and histogram with
#'                    a density plot. Note that partial plots are not available
#'                    for models with interaction terms.
#' @param resid       a character string specifying the type of residual used for
#'                    the partial (component-plus-residual) plots or Q-Q plot and
#'                    histogram, i.e., \code{"unstand"} for unstandardized residuals
#'                    \code{"stand"} for standardized residuals, and \code{"student"}
#'                    for studentized residual. By default, studentized residuals
#'                    are used for predicted values vs. residuals plot and unstandardized
#'                    residuals are used for Q-Q plot and histogram.
#' @param point.shape a numeric value for specifying the argument \code{shape}
#'                    in the \code{geom_point} function.
#' @param point.fill  a numeric value for specifying the argument \code{fill}
#'                    in the \code{geom_point} function.
#' @param point.size  a numeric value for specifying the argument \code{size}
#'                    in the \code{geom_point} function.
#' @param line1       logical: if \code{TRUE} (default), regression line is drawn
#'                    in the partial (component-plus-residual) plots, horizontal
#'                    line is drawn in the predicted values vs. residuals plot,
#'                    and t-distribution or normal distribution curve is drawn in
#'                    the histogram.
#' @param line2       logical: if \code{TRUE} (default), Loess smooth line is drawn
#'                    in the partial (component-plus-residual) plots, loess mooth
#'                    lines are drawn in the predicted values vs. residuals plot,
#'                    and density curve is drawn in the histogram.
#' @param line.type1  a character string or numeric value for specifying the argument
#'                    \code{linetype} in the \code{geom_smooth}, \code{geom_hline},
#'                    or \code{stat_function} function.
#' @param line.type2  a character string or numeric value for specifying the argument
#'                    \code{linetype} in the \code{geom_smooth} or \code{geom_density}
#'                     function.
#' @param line.width1 a numeric value for specifying the argument \code{linewidth}
#'                    in the \code{geom_smooth}, \code{geom_hline}, or \code{stat_function}
#'                    function.
#' @param line.width2 a numeric value for specifying the argument \code{linewidth}
#'                    in the \code{geom_smooth} or \code{geom_density} function.
#' @param line.color1 a character string or numeric value for specifying the argument
#'                    \code{color} in the \code{geom_smooth}, \code{geom_hline},
#'                    or \code{stat_function} function.
#' @param line.color2 a character string or numeric value for specifying the argument
#'                    \code{color} in the \code{geom_smooth} or \code{geom_density}
#'                     function.
#' @param bar.width   a numeric value for specifying the argument \code{binwidth}
#'                    in the \code{geom_bar} function.
#' @param bar.n       a numeric value for specifying the argument \code{bins} in
#'                    the \code{geom_bar} function.
#' @param bar.color   a character string or numeric value for specifying the argument
#'                    \code{color} in the \code{geom_bar} function.
#' @param bar.fill    a character string or numeric value for specifying the argument
#'                    \code{fill} in the \code{geom_bar} function.
#' @param strip.size  a numeric value for specifying the argument \code{size} in
#'                    the \code{element_text} function of the \code{strip.text}
#'                    argument within the \code{theme} function.
#' @param label.size  a numeric value for specifying the argument \code{size} in
#'                    the \code{element_text} function of the \code{axis.title}
#'                    argument within the \code{theme} function.
#' @param axis.size   a numeric value for specifying the argument \code{size} in
#'                    the \code{element_text} function of the \code{axis.text }
#'                    argument within the \code{theme} function.
#' @param xlimits     a numeric value for specifying the argument \code{limits}
#'                    in the \code{scale_x_continuous} function.
#' @param ylimits     a numeric value for specifying the argument \code{limits}
#'                    in the \code{scale_y_continuous} function.
#' @param xbreaks     a numeric value for specifying the argument \code{breaks }
#'                    in the \code{scale_x_continuous} function.
#' @param ybreaks     a numeric value for specifying the argument \code{breaks }
#'                    in the \code{scale_y_continuous} function.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param plot        logical: if \code{TRUE}, a plot is drawn.
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
#' In multiple regression, plotting the outcome variable \eqn{Y} against each predictor
#' variable \eqn{X} can be misleading because it does not reflect the partial
#' relationship between \eqn{Y} and \eqn{X} (i.e., statistically controlling for
#' the other \eqn{X}s), but rather the marginal relationship between \eqn{Y} and
#' \eqn{X} (i.e., ignoring the other \eqn{X}s). Partial residual plots or
#' component-plus-residual plots should be used to detect nonlinearity in multiple
#' regression. The partial residual for the \eqn{j}th predictor variable is defined
#' as
#'
#' \deqn{e_i^{(j)} = b_jX_{ij} + e_i}
#'
#' The linear component of the partial relationship between \eqn{Y} and \eqn{X_j}
#' is added back to the least-squares residuals, which may include an unmodeled
#' nonlinear component. Then, the partial residual \eqn{e_i^{(j)}} is plotted
#' against the predictor variable \eqn{X_j}. Nonlinearity may become apparent when
#' a non-parametric regression smoother is applied. By default, the function plots
#' each predictor against the partial residuals, and draws the linear regression
#' and the loess smooth line to the partial residual plots.}
#'
#' \item{\strong{Nonconstant Error Variance}}{The violation of the assumption of
#' constant error variance, often referred to as heteroscedasticity, implies that
#' the variance of the outcome variable around the regression surface is not the
#' same at every point on the regression surface (Fox, 2015).
#' Plotting residuals against the outcome variable \eqn{Y} instead of the predicted
#' values \eqn{\hat{Y}} is not recommended because \eqn{Y = \hat{Y} + e}. Consequently,
#' the linear correlation between the outcome variable \eqn{Y} and the residuals
#' \eqn{e} is \eqn{\sqrt{1 - R^2}} where \eqn{R} is the multiple correlation coefficient.
#' In contrast, plotting residuals against the predicted values \eqn{\hat{Y}} is
#' much easier to examine for evidence of nonconstant error variance as the correlation
#' between \eqn{\hat{Y}} and \eqn{e} is 0. Note that the least-squares residuals
#' generally have unequal variance \eqn{Var(e_i) = \sigma^2 / (1 - h_i)} where
#' \eqn{h} is the leverage of observation \eqn{i}, even if errors have constant
#' variance \eqn{\sigma^2}. The studentized residuals \eqn{e^*_i}, however, have
#' a constant variance under the assumption of the regression model. Residuals
#' are studentized by dividing them by \eqn{\sigma^2_i(\sqrt{(1 - h_i)}} where
#' \eqn{\sigma^2_i} is the estimate of \eqn{\sigma^2} obtained after deleting the
#' \eqn{i}th observation, and \eqn{h_i} is the leverage of observation \eqn{i}
#' (Meuleman et al, 2015).
#' By default, the function plots the predicted values
#' against the studentized residuals. It also draws a horizontal line at 0, a
#' loess smooth lines for all residuals as well as separate loess smooth lines
#' for positive and negative residuals.}
#' \item{\strong{Non-normality of Residuals}}{Statistical inference under the
#' violation of the assumption of normally distributed errors is approximately
#' valid in all but small samples. However, the efficiency of least squares is
#' not robust because the least-squares estimator is the most efficient and
#' unbiased estimator only when the errors are normally distributed. For instance,
#' when error distributions have heavy tails, the least-squares estimator becomes
#' much less efficient compared to robust estimators. In addition, error distributions
#' with heavy-tails result in outliers and compromise the interpretation of conditional
#' means because the mean is not an accurate measure of central tendency in a highly
#' skewed distribution. Moreover, a multimodal error distribution suggests the omission
#' of one or more discrete explanatory variables that naturally divide the data
#' into groups (Fox, 2016).
#' By default, the function plots a Q-Q plot of the unstandardized residuals, and
#' a histogram of the unstandardized residuals and a density plot. Note that
#' studentized residuals follow a \eqn{t}-distribution with \eqn{n - k - 2} degrees
#' of freedom where \eqn{n} is the sample size and \eqn{k} is the number of predictors.
#' However, the normal and \eqn{t}-distribution are nearly identical unless the
#' sample size is small. Moreover, even if the model is correct, the studentized
#' residuals are not an independent random sample from \eqn{t_{n - k - 2}}. Residuals
#' are correlated with each other depending on the configuration of the predictor
#' values. The correlation is generally negligible unless the sample size is small.}
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
#' Meuleman, B., Loosveldt, G., & Emonds, V. (2015). Regression analysis: Assumptions
#' and diagnostics. In H. Best & C. Wolf (Eds.), \emph{The SAGE handbook of regression analysis and causal inference (pp. 83-110)}.
#' Sage.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{model}}{model specified in \code{model}}
#' \item{\code{plotdat}}{data frame used for the plot}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{plot}}{ggplot2 object for plotting the residuals}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #----------------------------
#' # Residual diagnostics for a linear model
#' mod <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
#'
#' # Partial (component-plus-residual) plots
#' check.resid(mod, type = "linear")
#'
#' # Predicted values vs. residuals plot
#' check.resid(mod, type = "homo")
#'
#' # Q-Q plot and histogram with density plot
#' check.resid(mod, type = "normal")
#'
#' #----------------------------
#' # Extract data and ggplot2 object
#' object <- check.resid(mod, type = "linear", plot = FALSE)
#'
#' # Data frame
#' object$plotdat
#'
#' # ggplot object
#' object$plot
#' }
check.resid <- function(model, type = c("linear", "homo", "normal"),
                        resid = c("unstand", "stand", "student"),
                        point.shape = 21, point.fill = "gray80", point.size = 1,
                        line1 = TRUE, line2 = TRUE, line.type1 = "solid", line.type2 = "dashed",
                        line.width1 = 1, line.width2 = 1, line.color1 = "#0072B2", line.color2 = "#D55E00",
                        bar.width = NULL, bar.n = 30, bar.color = "black", bar.fill = "gray95",
                        strip.size = 11, label.size = 10, axis.size = 10,
                        xlimits = NULL, ylimits = NULL,
                        xbreaks = ggplot2::waiver(), ybreaks = ggplot2::waiver(),
                        check = TRUE, plot = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'model' is missing
  if (isTRUE(missing(model))) { stop("Input for the argument 'model' is missing.", call. = FALSE) }

  # Check if input 'model' is NULL
  if (isTRUE(is.null(model))) { stop("Input specified for the argument 'model' is NULL.", call. = FALSE) }

  # Check if input 'model' is not 'lm'
  if (isTRUE(class(model) != "lm")) { stop("Please specify an \"lm\" object for the argument 'model'.", call. = FALSE) }

  # Check if input 'model' has an interation term
  if (isTRUE(any(attr(terms(model), "order") > 1L) &&  all(type == "linear"))) { stop("Component-plus-residual plots are not available for models with interactions.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # R package ggplot2
    if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed for this function, please install the package.", call. = FALSE) }

    # R package patchwork
    if (isTRUE(all(type == "normal") && !nzchar(system.file(package = "patchwork")))) { stop("Package \"patchwork\" is needed for this function, please install the package.", call. = FALSE) }

    ## Check input 'type' ##
    if (isTRUE(!all(type %in% c("linear", "homo", "normal")))) { stop("Character string in the argument 'type' does not match with \"linear\", \"homo\", or \"normal\".", call. = FALSE) }

    ## Check input 'resid' ##
    if (isTRUE(!all(resid %in% c("unstand", "stand", "student")))) { stop("Character string in the argument 'type' does not match with \"unstand\", \"stand\", or \"student\".", call. = FALSE) }

    ## Check input 'line1' ##
    if (isTRUE(!is.logical(line1))) { stop("Please specify TRUE or FALSE for the argument 'line1'.", call. = FALSE) }

    ## Check input 'line2' ##
    if (isTRUE(!is.logical(line2))) { stop("Please specify TRUE or FALSE for the argument 'line2'.", call. = FALSE) }


    ## Check input 'plot' ##
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'plot'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Type ####

  if (isTRUE(all(c("linear", "homo", "normal") %in% type))) {

    type <- "homo"

  } else {

    if (isTRUE(length(type) != 1L)) { stop("Please specify a character string for the argument 'type'", call. = FALSE) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Residual ####

  if (isTRUE(all(c("unstand", "stand", "student") %in% resid))) {

    if (isTRUE(type != "normal")) { resid <- "student" } else { resid <- "unstand" }

  } else {

    if (isTRUE(length(resid) != 1L)) { stop("Please specify a character string for the argument 'resid'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # White background
  ggplot2::theme_set(ggplot2::theme_bw())

  pred <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Linearity ####

  switch(type, linear =  {

    #...................
    ### Predictor variables ####

    pred.names <- attr(model$terms, "term.labels")

    #...................
    ### Data frame ####

    plotdat <- data.frame(var = rep(pred.names, each = nrow(model$model)),
                          pred = unlist(model$model[, pred.names]),
                          resid = as.vector(residuals(model, "partial")),
                          row.names = NULL)

    #...................
    ### Plot data ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(pred, resid)) +
          ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill) +
          ggplot2::facet_wrap(~ var, scales = "free_x") +
          ggplot2::scale_x_continuous("Predictor", limits = xlimits, breaks = xbreaks) +
          ggplot2::scale_y_continuous("Residual", limits = ylimits, breaks = ybreaks) +
          ggplot2::theme(strip.text = ggplot2::element_text(size = strip.size),
                         axis.title = ggplot2::element_text(size = label.size),
                         axis.text = ggplot2::element_text(size = axis.size))

    #...................
    ### Add lines ####

    if (isTRUE(line1)) { p <- p +  ggplot2::geom_smooth(method = "lm", se = FALSE, formula = "y ~ x",
                                                        linetype = line.type1, linewidth = line.width1, color = line.color1) }

    if (isTRUE(line2)) { p <- p + ggplot2::geom_smooth(method = "loess", se = FALSE, formula = "y ~ x",
                                                       linetype = line.type2, linewidth = line.width2, color = line.color2) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Homoscedasticity ####

  }, homo = {

    #...................
    ### Residual ####

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

    #...................
    ### Limits and of the x and y-axis ####

    if (isTRUE(is.null(ylimits))) { ylimits <- c(-ceiling(max(abs(plotdat$resid))), ceiling(max(abs(plotdat$resid)))) }

    #...................
    ### Plot data ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(pred, resid)) +
          ggplot2::geom_point(size = point.size, shape = point.shape, fill = point.fill) +
          ggplot2::scale_x_continuous("Predicted Value", limits = xlimits, breaks = xbreaks) +
          ggplot2::scale_y_continuous(switch(resid,
                                             unstand = "Unstandardized Residual",
                                             stand = "Standardized Residual",
                                             student = "Studentized Residual"),
                                      limits = ylimits, breaks = ybreaks) +
          ggplot2::theme(strip.text = ggplot2::element_text(size = strip.size),
                         axis.title = ggplot2::element_text(size = label.size),
                         axis.text = ggplot2::element_text(size = axis.size))

    #...................
    ### Add lines ####

    if (isTRUE(line1)) { p <- p + ggplot2::geom_hline(yintercept = 0, linetype = line.type1, linewidth = line.width1, color = line.color1) }

    if (isTRUE(line2)) { p <-  p + ggplot2::geom_smooth(method = loess, se = FALSE, formula = "y ~ x",
                                                        linetype = line.type2, linewidth = line.width2, color = line.color2) +
                                   ggplot2::geom_smooth(data = subset(plotdat, resid > 0L), method = loess, formula = "y ~ x",
                                                        se = FALSE, linetype = line.type2, linewidth = line.width2, color = line.color2) +
                                   ggplot2::geom_smooth(data = subset(plotdat, resid < 0L), method = loess, formula = "y ~ x",
                                                        se = FALSE, linetype = line.type2, linewidth = line.width2, color = line.color2) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Normality of Residuals ####

  }, normal =  {

    #...................
    ### Residual ####

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

    #...................
    ### Q-Q Plot ####

    p1 <- ggplot2::ggplot(plotdat, ggplot2::aes(sample = resid)) +
            ggplot2::stat_qq(size = point.size, shape = point.shape, fill = point.fill) +
            ggplot2::scale_x_continuous(if (isTRUE(resid == "student")) { "Theoretical Quantiles of the t Distribution"  } else { "Theoretical Quantiles of the Normal Distribution" },
                                        limits = xlimits, breaks = xbreaks) +
            ggplot2::scale_y_continuous(switch(resid,
                                               unstand = "Sample Quantile of Unstandardized Residuals",
                                               stand = "Sample Quantile of Standardized Residual",
                                               student = "Sample Quantile of Studentized Residual"),
                                        limits = ylimits, breaks = ybreaks) +
            ggplot2::theme(strip.text = ggplot2::element_text(size = strip.size),
                           axis.title = ggplot2::element_text(size = label.size),
                           axis.text = ggplot2::element_text(size = axis.size))

    # Add line
    if (isTRUE(resid == "student")) {

      p1 <- p1 + ggplot2::stat_qq_line(distribution = qt, dparams = length(plotdat$resid) - length(attr(model$terms, "term.labels")) - 2L)

    } else {

      p1 <- p1 + ggplot2::stat_qq_line()

    }

    #...................
    ### Histogram ####

    # Limits of the x-axis
    if (isTRUE(is.null(xlimits))) { xlimits <- c(-ceiling(max(abs(plotdat$resid))), ceiling(max(abs(plotdat$resid)))) }

    # Plot data
    p2 <- ggplot2::ggplot(plotdat, ggplot2::aes(resid)) +
            ggplot2::geom_histogram(ggplot2::aes(y = ggplot2::after_stat(density)),
                                                 binwidth = bar.width, bins = bar.n, color = bar.color, fill = bar.fill) +
            ggplot2::scale_x_continuous(switch(resid,
                                               unstand = "Unstandardized Residual",
                                               stand = "Standandardized Residual",
                                               student = "Studentized Residual"),
                                        limits = xlimits, breaks = xbreaks) +
            ggplot2::scale_y_continuous("Density", limits = ylimits, breaks = ybreaks) +
            ggplot2::theme(strip.text = ggplot2::element_text(size = strip.size),
                           axis.title = ggplot2::element_text(size = label.size),
                           axis.text = ggplot2::element_text(size = axis.size))

    #...................
    ### Add lines ####

    if (isTRUE(line1)) { p2 <- p2 + ggplot2::stat_function(fun = if (isTRUE(resid == "student")) { dt } else { dnorm },
                                                           args = if (isTRUE(resid == "student")) { list(df = length(na.omit(plotdat$resid)) - length(attr(model$terms, "term.labels")) - 2L) } else { list(mean = mean(plotdat$resid, na.rm = TRUE), sd = sd(plotdat$resid, na.rm = TRUE)) },
                                                           col = line.color1, linetype = line.type1, linewidth = line.width1) }

    if (isTRUE(line2)) { p2 <- p2 + ggplot2::geom_density(color = line.color2, linetype = line.type2, linewidth = line.width2) }

    #...................
    ### Arrange plots ####

    p <- patchwork::wrap_plots(p1, p2, ncol = 2L)

  })

  #_____________________________________________________________________________
  #
  # Return object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "check.resid",
                 model = model,
                 plotdat = plotdat,
                 args = list(type = type, resid = resid,
                             point.shape = point.shape, point.fill = point.fill, point.size = point.size,
                             line1 = line1, line2 = line2, line.type1 = line.type1, line.type2 = line.type2,
                             line.width1 = line.width1, line.width2 = line.width2, line.color1 = line.color1, line.color2 = line.color2,
                             bar.width = bar.width, bar.n = bar.n, bar.color = bar.color, bar.fill = bar.fill,
                             strip.size = strip.size, label.size = label.size, axis.size = axis.size,
                             xlimits = xlimits, ylimits = ylimits, xbreaks = xbreaks, ybreaks = ybreaks,
                             check = check, plot = plot),
                 plot = p)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot -----------------------------------------------------------------------

  if (isTRUE(plot)) { invisible(suppressMessages(suppressWarnings(print(p)))) }

  return(invisible(object))

}
