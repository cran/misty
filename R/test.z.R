#' z-Test
#'
#' This function performs one-sample, two-sample, and paired-sample z-tests and
#' provides descriptive statistics, effect size measure, and a plot showing error
#' bars for (difference-adjusted) confidence intervals with jittered data points.
#'
#' Cohen's d reported when argument \code{effsize = TRUE} is based on the population
#' standard deviation specified in \code{sigma} or the square root of the population
#' variance specified in \code{sigma2}. In a one-sample and paired-sample design,
#' Cohen's d is the mean of the difference scores divided by the population standard
#' deviation of the difference scores (i.e., equivalent to Cohen's \eqn{d_z} according
#' to Lakens, 2013). In a two-sample design, Cohen's d is the difference between
#' means of the two groups of observations divided by either the population standard
#' deviation when assuming and specifying equal standard deviations or the unweighted
#' pooled population standard deviation when assuming and specifying unequal standard
#' deviations.
#'
#' @param x              a numeric vector of data values.
#' @param y              a numeric vector of data values.
#' @param sigma          a numeric vector indicating the population standard deviation(s).
#'                       In case of two-sample z-test, equal standard deviations are
#'                       assumed when specifying one value for the argument \code{sigma};
#'                       when specifying two values for the argument \code{sigma},
#'                       unequal standard deviations are assumed. Note that either
#'                       argument \code{sigma} or argument \code{sigma2} is specified.
#' @param sigma2         a numeric vector indicating the population variance(s). In
#'                       case of two-sample z-test, equal variances are assumed when
#'                       specifying one value for the argument \code{sigma2}; when
#'                       specifying two values for the argument \code{sigma}, unequal
#'                       variance are assumed. Note that either argument \code{sigma}
#'                       or argument \code{sigma2} is specified.
#' @param mu             a numeric value indicating the population mean under the null
#'                       hypothesis. Note that the argument \code{mu} is only used
#'                       when computing a one-sample z-test.
#' @param paired         logical: if \code{TRUE}, paired-sample z-test is computed.
#' @param alternative    a character string specifying the alternative hypothesis,
#'                       must be one of \code{"two.sided"} (default), \code{"greater"}
#'                       or \code{"less"}.
#' @param hypo           logical: if \code{TRUE} (default), null and alternative
#'                       hypothesis are shown on the console.
#' @param descript       logical: if \code{TRUE} (default), descriptive statistics
#'                       are shown on the console.
#' @param effsize        logical: if \code{TRUE}, effect size measure Cohen's d is
#'                       shown on the console.
#' @param conf.level     a numeric value between 0 and 1 indicating the confidence
#'                       level of the interval.
#' @param digits         an integer value indicating the number of decimal places
#'                       to be used for displaying descriptive statistics and
#'                       confidence interval.
#' @param p.digits       an integer value indicating the number of decimal places
#'                       to be used for displaying the \emph{p}-value.
#' @param as.na          a numeric vector indicating user-defined missing values,
#'                       i.e. these values are converted to \code{NA} before
#'                       conducting the analysis.
#' @param plot           logical: if \code{TRUE}, a plot showing bar plots with
#'                       error bars for confidence intervals is drawn.
#' @param bar            logical: if \code{TRUE} (default), bars representing means
#'                       for each groups are drawn.
#' @param point          logical: if \code{TRUE}, points representing means for
#'                       each groups are drawn.
#' @param ci             logical: if \code{TRUE} (default), error bars representing
#'                       confidence intervals are drawn.
#' @param jitter         logical: if \code{TRUE}, jittered data points are drawn.
#' @param line           logical: if \code{TRUE} (default), a horizontal line
#'                       is drawn at \code{mu} for the one-sample z-test or at
#'                       0 for the paired-sample z-test.
#' @param adjust         logical: if \code{TRUE} (default), difference-adjustment
#'                       for the confidence intervals in a two-sample design is
#'                       applied.
#' @param point.size     a numeric value indicating the \code{size} aesthetic for
#'                       the point representing the mean value.
#' @param errorbar.width a numeric value indicating the horizontal bar width of
#'                       the error bar.
#' @param xlab           a character string specifying the labels for the x-axis.
#' @param ylab           a character string specifying the labels for the y-axis.
#' @param ylim           a numeric vector of length two specifying limits of the
#'                       limits of the y-axis.
#' @param ybreaks        a numeric vector specifying the points at which tick-marks
#'                       are drawn at the y-axis.
#' @param linetype       an integer value or character string specifying the line
#'                       type for the line representing the population mean under
#'                       the null hypothesis, i.e., 0 = blank, 1 = solid, 2 = dashed,
#'                       3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash.
#' @param linewidth      a numeric value indicating the \code{linewidth} aesthetic
#'                       for the line representing the population mean under the
#'                       null hypothesis.
#' @param jitter.size    a numeric value indicating the \code{size} aesthetic
#' @param jitter.width   a numeric value indicating the amount of horizontal jitter.
#' @param jitter.height  a numeric value indicating the amount of vertical jitter.
#' @param jitter.alpha   a numeric value between 0 and 1 for specifying the
#'                       \code{alpha} argument in the \code{geom_jitter}
#'                       function for controlling the opacity of the jittered
#'                       data points.
#' @param title          a character string specifying the text for the title for
#'                       the plot.
#' @param subtitle       a character string specifying the text for the subtitle for
#'                       the plot.
#' @param filename       a character string indicating the \code{filename}
#'                       argument including the file extension in the \code{ggsave}
#'                       function. Note that one of \code{".eps"}, \code{".ps"},
#'                       \code{".tex"}, \code{".pdf"} (default),
#'                       \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                       \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                       to be specified as file extension in the \code{file}
#'                       argument. Note that plots can only be saved when
#'                       \code{plot = TRUE}.
#' @param width          a numeric value indicating the \code{width} argument
#'                       (default is the size of the current graphics device)
#'                       for the \code{ggsave} function.
#' @param height         a numeric value indicating the \code{height} argument
#'                       (default is the size of the current graphics device)
#'                       for the \code{ggsave} function.
#' @param units          a character string indicating the \code{units} argument
#'                       (default is \code{in}) for the \code{ggsave} function.
#' @param dpi            a numeric value indicating the \code{dpi} argument
#'                       (default is \code{600}) for the \code{ggsave} function.
#' @param write          a character string naming a text file with file extension
#'                       \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                       output into a text file.
#' @param append         logical: if \code{TRUE} (default), output will be appended
#'                       to an existing text file with extension \code{.txt} specified
#'                       in \code{write}, if \code{FALSE} existing text file will be
#'                       overwritten.
#' @param check          logical: if \code{TRUE} (default), argument specification
#'                       is checked.
#' @param output         logical: if \code{TRUE} (default), output is shown on the
#'                       console.
#' @param formula        in case of two sample z-test (i.e., \code{paired = FALSE}),
#'                       a formula of the form \code{y ~ group} where \code{group}
#'                       is a numeric variable, character variable or factor with
#'                       two values or factor levels giving the corresponding
#'                       groups.
#' @param data           a matrix or data frame containing the variables in the
#'                       formula \code{formula}.
#' @param ...            further arguments to be passed to or from methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{test.t}}, \code{\link{aov.b}}, \code{\link{aov.w}}, \code{\link{test.welch}},
#' \code{\link{cohens.d}}, \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}
#'
#' @references
#' Lakens, D. (2013). Calculating and reporting effect sizes to facilitate cumulative
#' science: A practical primer for t-tests and ANOVAs. \emph{Frontiers in Psychology, 4},
#' 1-12. https://doi.org/10.3389/fpsyg.2013.00863
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{sample}}{type of sample, i.e., one-, two-, or paired sample}
#' \item{\code{formula}}{formula}
#' \item{\code{data}}{data frame with the outcome and grouping variable}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{result}}{result table}
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # One-Sample Design
#'
#' # Example 1a: Two-sided one-sample z-test, population mean = 20, population SD = 6
#' test.z(mtcars$mpg, sigma = 6, mu = 20)
#'
#' # Example 1b: One-sided one-sample z-test, population mean = 20, population SD = 6,
#' # print Cohen's d
#' test.z(mtcars$mpg, sigma = 6, mu = 20, alternative = "greater", effsize = TRUE)
#'
#' # Example 1c: Two-sided one-sample z-test, population mean = 20, population SD = 6,
#' # plot results
#' test.z(mtcars$mpg, sigma = 6, mu = 20, plot = TRUE)
#'
#' \dontrun{
#' # Example 1d: Two-sided one-sample z-test, save plot
#' test.z(mtcars$mpg, sigma = 6, mu = 20, plot = TRUE, filename = "One-sample_z-test.png",
#'        width = 4, height = 5)
#' }
#'
#' #----------------------------------------------------------------------------
#' # Two-Sample Design
#'
#' # Example 2a: Two-sided two-sample z-test, population SD = 6, equal SD assumption
#' test.z(mpg ~ vs, data = mtcars, sigma = 6)
#'
#' # Example 2b: Two-sided two-sample z-test, alternative specification
#' test.z(c(3, 1, 4, 2, 5, 3, 6, 7), c(5, 2, 4, 3, 1), sigma = 1.2)
#'
#' # Example 2c: Two-sided two-sample z-test, population SD = 4 and 6, unequal SD assumption
#' test.z(mpg ~ vs, data = mtcars, sigma = c(4, 6))
#'
#' # Example 2d: One-sided two-sample z-test, population SD = 4 and 6, unequal SD assumption
#' # print Cohen's d
#' test.z(mpg ~ vs, data = mtcars, sigma = c(4, 6), alternative = "greater",
#'        effsize = TRUE)
#'
#' # Example 2e: Two-sided two-sample z-test, population SD = 6, equal SD assumption
#' # plot results
#' test.z(mpg ~ vs, data = mtcars, sigma = 6, plot = TRUE)
#'
#' \dontrun{
#' # Example 2f: Two-sided two-sample z-test, save plot
#' test.z(mpg ~ vs, data = mtcars, sigma = 6, plot = TRUE, filename = "Two-sample_z-test.png",
#'        width = 5, height = 6)
#' }
#'
#' #----------------------------------------------------------------------------
#' # Paired-Sample Design
#'
#' # Example 3a: Two-sided paired-sample z-test, population SD of difference score = 1.2
#' test.z(mtcars$drat, mtcars$wt, sigma = 1.2, paired = TRUE)
#'
#' # Example 3b: One-sided paired-sample z-test, population SD of difference score = 1.2,
#' # print Cohen's d
#' test.z(mtcars$drat, mtcars$wt, sigma = 1.2, paired = TRUE,
#'        alternative = "greater", effsize = TRUE)
#'
#' # Example 3c: Two-sided paired-sample z-test, population SD of difference score = 1.2,
#' # plot results
#' test.z(mtcars$drat, mtcars$wt, sigma = 1.2, paired = TRUE, plot = TRUE)
#'
#' \dontrun{
#' # Example 3d: Two-sided paired-sample z-test, save plot
#' test.z(mtcars$drat, mtcars$wt, sigma = 1.2, paired = TRUE, plot = TRUE,
#'        filename = "Paired-sample_z-test.png", width = 4, height = 5)
#' }
test.z <- function(x, ...) {

  UseMethod("test.z")

}

#_______________________________________________________________________________
#
# Default S3 method ------------------------------------------------------------

test.z.default <- function(x, y = NULL, sigma = NULL, sigma2 = NULL, mu = 0,
                           paired = FALSE, alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95, hypo = TRUE, descript = TRUE, effsize = FALSE,
                           digits = 2, p.digits = 3, as.na = NULL, plot = FALSE, bar = TRUE,
                           point = FALSE, ci = TRUE, line = TRUE, jitter = FALSE,
                           adjust = TRUE, point.size = 4, errorbar.width = 0.1, xlab = NULL,
                           ylab = NULL, ylim = NULL, ybreaks = ggplot2::waiver(), linetype = 3,
                           linewidth = 0.8, jitter.size = 1.25, jitter.width = 0.05, jitter.height = 0,
                           jitter.alpha = 0.1, title = "", subtitle = "Confidence Interval",
                           filename = NULL, width = NA, height = NA, units = c("in", "cm", "mm", "px"),
                           dpi = 600, write = NULL, append = TRUE, check = TRUE,
                           output = TRUE, ...) {

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(sigma) && is.null(sigma2))) { stop("Please specify either argument 'sigma' or argument 'sigma2'.", call. = FALSE) }

  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1L) { stop("More than one variable specified for the argument 'x'.",call. = FALSE) }

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  if (!is.null(y)) {

    # Check if only one variable specified in the input 'y'
    if (ncol(data.frame(y)) != 1L) { stop("More than one variable specified for the argument 'y'.",call. = FALSE) }

    # Convert 'y' into a vector
    y <- unlist(y, use.names = FALSE)

  }

  # Check input 'paired'
  if (isTRUE(!is.logical(paired))) { stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE) }


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # One sample
    if (isTRUE(is.null(y))) {

    # Replace user-specified values with missing values
    x <- .as.na(x, na = as.na)

    # Two or paired sample
    } else {

      # Replace user-specified values with missing values
      x <- .as.na(x, na = as.na)
      y <- .as.na(y, na = as.na)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Paired sample ####

  if (isTRUE(is.null(y) && isTRUE(paired))) {

    # Length of 'x' and 'y'
    if (isTRUE(length(x) != length(y))) { stop("Length of the vector specified in 'x' does not match the length of the vector specified in 'y'.", call. = FALSE) }

    # Listwise deletion
    if (isTRUE(nrow(na.omit(data.frame(x = x, y = y))) < 2L)) { stop("After listwise deletion, the number of pairs of observations is less than two.",call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("hypo", "descript", "effsize", "plot", "adjust", "line", "jitter", "append", "output"),
               numeric = list(mu = 1L, point.size = 1L, errorbar.width = 1L, ylim = 2L, linewidth = 1L, jitter.size = 1L, jitter.width = 1L, jitter.height = 1L, jitter.alpha = 1L),
               character = list(xlab = 1L, ylab = 1L, title = 1L, subtitle = 1L),
               args = c("alternative", "conf.level", "digits", "p.digits", "write1"),
               envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Package ggplot2
    if (isTRUE(plot)) { if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed to draw a plot, please install the package.", call. = FALSE) } }

    # Check input 'sigma' and 'sigma2'
    if (isTRUE(!is.null(sigma) && !is.null(sigma2))) { if (isTRUE(!identical(sigma^2, sigma2))) { stop("Arguments 'sigma' and 'sigma2' do not match.", call. = FALSE) } }

    # Check input 'sigma'
    if (isTRUE(!is.null(sigma))) {

      # SD smaller or equal 0
      if (isTRUE(any(sigma <= 0L))) { stop("Please specify numeric values grater than 0 for the argument 'sigma'.", call. = FALSE) }

      # One sample
      if (isTRUE(is.null(y))) {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 1L)) { stop("Please specify one numeric values for the argument 'sigma' for one sample.", call. = FALSE) }

      # Two samples
      } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 2L)) { stop("Please specify one or two numeric values for the argument 'sigma' in independent samples.", call. = FALSE) }

      # Paired samples
      } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 1L)) { stop("Please specify one numeric values for the argument 'sigma' in paired samples.", call. = FALSE) }

      }

    }

    # Check input 'sigma2'
    if (isTRUE(!is.null(sigma2))) {

      # Variance smaller or equal 0
      if (isTRUE(any(sigma2 <= 0L))) { stop("Please specify numeric values grater than 0 for the argument 'sigma2'.", call. = FALSE) }

      if (!isTRUE(paired)) {

        # Length of 'sigma2'
        if (length(sigma2) > 2L) { stop("Please specify one or two numeric values for the argument 'sigma2' in paired samples.", call. = FALSE) }

      } else {

        # Length of 'sigma2'
        if (isTRUE(length(sigma2) > 1L)) { stop("Please specify one numeric values for the argument 'sigma2' in dependent samples.", call. = FALSE) }

      }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Global variables
  m <- m.low <- m.upp <- group <- low <- upp <- m.diff <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Population standard deviation and variance ####

  if (isTRUE(is.null(sigma) && !is.null(sigma2))) { sigma <- sqrt(sigma2) }

  if (isTRUE(!is.null(sigma) && is.null(sigma2))) { sigma2 <- sigma^2 }

  #...................
  ### Two-sample design ####

  if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    if (isTRUE(!is.null(sigma) && length(sigma) == 1L)) { sigma <- c(sigma, sigma) }

    if (isTRUE(!is.null(sigma2) && length(sigma2) == 1L)) { sigma2 <- c(sigma2, sigma2) }

  }

  #...................
  ### Alternative hypothesis ####

  if (all(c("two.sided", "less", "greater") %in% alternative)) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One-sample design ####
  if (isTRUE(is.null(y))) {

    # Descriptive statistics
    x.ci <- misty::ci.mean(x, sigma = sigma, alternative = alternative, output = FALSE)$result

    # Standard error of the mean
    se <- (sigma / sqrt(x.ci[["n"]]))

    # Test statistic
    z <- (x.ci[["m"]] - mu) / se

    # Cohen's d
    d <- (x.ci[["m"]] - mu) / sigma

    result <- data.frame(n = x.ci[["n"]], nNA = x.ci[["nNA"]],
                         m = x.ci[["m"]], sd = x.ci[["sd"]],
                         m.diff = x.ci[["m"]] - mu, se = se,
                         m.low = x.ci[["low"]], m.upp = x.ci[["upp"]], z = z,
                         pval = switch(alternative, two.sided = pnorm(abs(z), lower.tail = FALSE) * 2, less = pnorm(z, lower.tail = TRUE), greater = pnorm(z, lower.tail = FALSE)),
                          d = d, row.names = NULL)

    sample <- "one"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two samples design ####
  } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    # Descriptive statistics
    x.ci <- misty::df.rename(misty::ci.mean.diff(x = x, y = y, sigma = sigma, alternative = alternative, output = FALSE)$result,
                             from = c("between", "low", "upp"), to = c("group", "m.low", "m.upp"))

    # Standard error of the mean difference
    se <- sqrt((sigma2[1L] / x.ci[1L, "n"]) + (sigma2[2L] / x.ci[2L, "n"]))

    # Test statistic
    z <- x.ci[2L, "m.diff"] / se

    # Cohen's d
    d <- x.ci[2L, "m.diff"] / mean(sigma)

    result <- data.frame(cbind(x.ci[, c("group", "n", "nNA", "m", "sd", "m.diff")],
                               se = c(NA, se), x.ci[, c("m.low", "m.upp")], z = c(NA, z),
                               pval = c(NA, switch(alternative, two.sided = pnorm(abs(z), lower.tail = FALSE) * 2L, less = pnorm(z, lower.tail = TRUE), greater = pnorm(z, lower.tail = FALSE))),
                               d = c(NA, d)), row.names = NULL)

    sample <- "two"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Paired samples ####
  } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

    # Descriptive statistics
    x.ci <- misty::ci.mean.diff(x = x, y = y, sigma = sigma, paired = TRUE, alternative = alternative, output = FALSE)$result

    # Standard error of the mean difference
    se <- (sigma / sqrt(x.ci[["n"]]))

    # Test statistic
    z <- (x.ci[["m.diff"]]) / se

    # Cohen's d
    d <- (x.ci[["m.diff"]]) / sigma

    result <- data.frame(n  = x.ci[["n"]], nNA = x.ci[["nNA"]],
                         m1 = x.ci[["m1"]], m2 = x.ci[["m2"]],
                         m.diff = x.ci[["m.diff"]], sd.diff = x.ci[["sd.diff"]],
                         se = se, m.low = x.ci[["low"]], m.upp = x.ci[["upp"]], z = z,
                         pval = switch(alternative, two.sided = pnorm(abs(z), lower.tail = FALSE) * 2L, less = pnorm(z, lower.tail = TRUE), greater = pnorm(z, lower.tail = FALSE)),
                         d = d, row.names = NULL)


    sample <- "paired"

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "test.z",
                 sample = sample,
                 data = list(x = x, y = y),
                 args = list(sigma = sigma, sigma2 = sigma2, mu = mu, paired = paired,
                             alternative = alternative, conf.level = conf.level,
                             hypo = hypo, descript = descript, effsize = effsize,
                             digits = 2, p.digits = 3, as.na = NULL, plot = plot, bar = bar,
                             point = point, ci = ci, line = line, jitter = jitter,
                             adjust = adjust, point.size = point.size, errorbar.width = errorbar.width,
                             xlab = xlab, ylab = ylab, ylim = ylim, ybreaks = ybreaks,
                             linetype = linetype, linewidth = linewidth, jitter.size = jitter.size,
                             jitter.width = jitter.width, jitter.height = jitter.height,
                             jitter.alpha = jitter.alpha, title = title, subtitle = subtitle,
                             filename = filename, width = width, height = height, units = units,
                             dpi = dpi, write = write, append = append, check = check, output = output),
                 plot = NULL, result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Results ------------------------------------------------------

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, units = units, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to textfile
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
#
# S3 method for class 'formula' ------------------------------------------------

test.z.formula <- function(formula, data, sigma = NULL, sigma2 = NULL,
                           alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95, hypo = TRUE, descript = TRUE, effsize = FALSE,
                           digits = 2, p.digits = 3, as.na = NULL, plot = FALSE, bar = TRUE,
                           point = FALSE, ci = TRUE, line = TRUE, jitter = FALSE,
                           adjust = TRUE, point.size = 4, errorbar.width = 0.1,
                           xlab = NULL, ylab = NULL, ylim = NULL, ybreaks = ggplot2::waiver(),
                           linetype = 3, linewidth = 0.8, jitter.size = 1.25, jitter.width = 0.05,
                           jitter.height = 0, jitter.alpha = 0.1, title = "",
                           subtitle = "Confidence Interval", filename = NULL,
                           width = NA, height = NA, units = c("in", "cm", "mm", "px"),
                           dpi = 600, write = NULL, append = TRUE, check = TRUE,
                           output = TRUE, ...) {

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Formula ####

  #...................
  ### Variables ####

  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome(s)
  y.vars <- var.formula[-grep(group.var, var.formula)]

  #...................
  ### Check ####

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check if variables are in the data
    (!var.formula %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(y)], collapse = ", ")), call. = FALSE) })()

    # Check if input 'formula' has only one grouping variable
    if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

    # Check if input 'formula' has only one outcome variable
    if (isTRUE(length(y.vars) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Global variables
  group <- m <- low <- upp <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  data.split <- split(unlist(data[, y.vars]), f = unlist(data[, group.var]))

  object <- test.z.default(x = data.split[[1L]], y = data.split[[2L]],
                           sigma = sigma, sigma2 = sigma2, alternative = alternative,
                           conf.level = conf.level, hypo = hypo, descript = descript,
                           effsize = effsize, plot = FALSE, bar = bar, point = point,
                           ci = ci, line = line, jitter = jitter, adjust = adjust,
                           point.size = point.size, errorbar.width = errorbar.width,
                           xlab = xlab, ylab = ylab, ylim = ylim, ybreaks = ybreaks,
                           linetype = linetype, linewidth = linewidth, jitter.size = jitter.size,
                           jitter.width = jitter.width, jitter.height = jitter.height,
                           jitter.alpha = jitter.alpha, title = title, subtitle = subtitle,
                           filename = filename, width = width, height = height, units = units,
                           dpi = dpi, write = write, append = append, check = check, output = FALSE)

  object$result[, "group"] <- names(data.split)

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "test.z",
                 sample = "two",
                 data = data[, var.formula],
                 formula = formula,
                 args = list(sigma = object$args$sigma, sigma2 = object$args$sigma2,
                             alternative = alternative, conf.level = conf.level,
                             hypo = hypo, descript = descript, effsize = effsize,
                             digits = digits, p.digits = p.digits, as.na = as.na,
                             plot = plot, bar = bar, point = point, ci = ci, line = line, jitter = jitter,
                             adjust = adjust, point.size = point.size, errorbar.width = errorbar.width,
                             xlab = xlab, ylab = ylab, ylim = ylim, ybreaks = ybreaks,
                             linetype = linetype, linewidth = linewidth, jitter.size = jitter.size,
                             jitter.width = jitter.width, jitter.height = jitter.height,
                             jitter.alpha = jitter.alpha, title = title, subtitle = subtitle,
                             filename = filename, width = width, height = height, units = units,
                             dpi = dpi, write = write, append = append, check = check, output = output),
                 plot = NULL, result = object$result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Results ------------------------------------------------------

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, units = units, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to text file
    sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

    if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

    # Print object
    print(object, check = FALSE)

    # Close file connection
    sink()

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
