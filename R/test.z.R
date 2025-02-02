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
#' @param plot           logical: if \code{TRUE}, a plot showing error bars for
#'                       confidence intervals is drawn.
#' @param point.size     a numeric value indicating the \code{size} aesthetic for
#'                       the point representing the mean value.
#' @param adjust         logical: if \code{TRUE} (default), difference-adjustment
#'                       for the confidence intervals a two-sample design is applied.
#' @param errorbar.width a numeric value indicating the horizontal bar width of
#'                       the error bar.
#' @param xlab           a character string specifying the labels for the x-axis.
#' @param ylab           a character string specifying the labels for the y-axis.
#' @param ylim           a numeric vector of length two specifying limits of the
#'                       limits of the y-axis.
#' @param breaks         a numeric vector specifying the points at which tick-marks
#'                       are drawn at the y-axis.
#' @param line           logical: if \code{TRUE} (default), a horizontal line
#'                       is drawn at \code{mu} for the one-sample t-test or at
#'                       0 for the paired-sample t-test.
#' @param linetype      an integer value or character string specifying the line
#'                       type for the line representing the population mean under
#'                       the null hypothesis, i.e., 0 = blank, 1 = solid, 2 = dashed,
#'                       3 = dotted, 4 = dotdash, 5 = longdash, 6 = twodash.
#' @param linewidth      a numeric value indicating the \code{size} aesthetic
#'                       for the line representing the population mean under the
#'                       null hypothesis.
#' @param jitter         logical: if \code{TRUE} (default), jittered data points
#'                       are drawn.
#' @param jitter.size    a numeric value indicating the \code{size} aesthetic
#'                       for the jittered data points.
#' @param jitter.width   a numeric value indicating the amount of horizontal jitter.
#' @param jitter.height  a numeric value indicating the amount of vertical jitter.
#' @param jitter.alpha   a numeric value indicating the opacity of the jittered
#'                       data points.
#' @param title          a character string specifying the text for the title for
#'                       the plot.
#' @param subtitle       a character string specifying the text for the subtitle
#'                       for the plot.
#' @param digits         an integer value indicating the number of decimal places
#'                       to be used for displaying descriptive statistics and
#'                       confidence interval.
#' @param p.digits       an integer value indicating the number of decimal places
#'                       to be used for displaying the \emph{p}-value.
#' @param as.na          a numeric vector indicating user-defined missing values,
#'                       i.e. these values are converted to \code{NA} before
#'                       conducting the analysis.
#' @param write          a character string naming a text file with file extension
#'                       \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                       output into a text file.
#' @param append         logical: if \code{TRUE} (default), output will be appended
#'                       to an existing text file with extension \code{.txt} specified
#'                       in \code{write}, if \code{FALSE} existing text file will be
#'                        overwritten.
#' @param check          logical: if \code{TRUE} (default), argument specification
#'                       is checked.
#' @param output         logical: if \code{TRUE} (default), output is shown on the
#'                       console.
#' @param formula        in case of two sample z-test (i.e., \code{paired = FALSE}),
#'                       a formula of the form \code{y ~ group} where \code{group}
#'                       is a numeric variable, character variable
#'                       or factor with two values or factor levels giving the
#'                       corresponding groups.
#' @param data           a matrix or data frame containing the variables in the
#'                      formula \code{formula}.
#' @param ...           further arguments to be passed to or from methods.
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
#' # Example 1a: Two-sided one-sample z-test
#' # population mean = 20, population standard deviation = 6
#' test.z(mtcars$mpg, sigma = 6, mu = 20)
#'
#' # Example 1b: One-sided one-sample z-test
#' # population mean = 20, population standard deviation = 6
#' # print Cohen's d
#' test.z(mtcars$mpg, sigma = 6, mu = 20, alternative = "greater", effsize = TRUE)
#'
#' # Example 1c: Two-sided one-sample z-test
#' # population mean = 20, population standard deviation = 6
#' # plot results
#' test.z(mtcars$mpg, sigma = 6, mu = 20, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Save plot, ggsave() from the ggplot2 package
#' ggsave("One-sample_z-test.png", dpi = 600, width = 3, height = 6)
#'
#' # Example 1d: Two-sided one-sample z-test
#' # population mean = 20, population standard deviation = 6
#' # extract plot and results
#' p <- test.z(mtcars$mpg, sigma = 6, mu = 20, output = FALSE)$plot
#' p
#'
#' # Example 1e: Two-sided one-sample z-test
#' # Draw plot in line with the default setting of test.z()
#'
#' # Extract data
#' plotdat <- data.frame(test.z(mtcars$mpg, sigma = 6, mu = 20, output = FALSE)$data[[1]])
#'
#' # Extract results
#' result <- test.z(mtcars$mpg, sigma = 6, mu = 20, output = FALSE)$result
#'
#' # Draw plot
#' ggplot(plotdat, aes(0, x)) +
#'   geom_point(data = result, aes(x = 0L, m), size = 4) +
#'   geom_errorbar(data = result, aes(x = 0L, y = m, ymin = m.low, ymax = m.upp),
#'                 width = 0.2) +
#'   scale_x_continuous(name = NULL, limits = c(-2, 2)) +
#'   scale_y_continuous(name = NULL) +
#'   geom_hline(yintercept = 20, linetype = 3, linewidth = 0.8) +
#'   labs(subtitle = "Two-Sided 95% Confidence Interval") +
#'   theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5),
#'                      axis.text.x = element_blank(),
#'                      axis.ticks.x = element_blank())
#'
#' #----------------------------------------------------------------------------
#' # Two-Sample Design
#'
#' # Example 2a: Two-sided two-sample z-test
#' # population standard deviation (SD) = 62, equal SD assumption
#' test.z(mpg ~ vs, sigma = 6, data = mtcars)
#'
#' # Example 2b: Two-sided two-sample z-test
#' # population standard deviation (SD) = 4 and 6, unequal SD assumption
#' test.z(mpg ~ vs, sigma = c(4, 6), data = mtcars)
#'
#' # Example 2c: One-sided two-sample z-test
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' # print Cohen's d
#' test.z(mpg ~ vs, sigma = c(4, 6), data = mtcars, alternative = "greater",
#'        effsize = TRUE)
#'
#' # Example 2d: Two-sided two-sample z-test
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' # plot results
#' test.z(mpg ~ vs, sigma = 6, data = mtcars, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Save plot, ggsave() from the ggplot2 package
#' ggsave("Two-sample_z-test.png", dpi = 600, width = 4, height = 6)
#'
#' # Example 2e: Two-sided two-sample z-test
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' # extract plot
#' p <- test.z(mpg ~ vs, sigma = 6, data = mtcars, output = FALSE)$plot
#' p
#'
#' # Example 2f: Two-sided two-sample z-test
#' # population standard deviation (SD) = 1.2, equal SD assumption
#' test.z(c(3, 1, 4, 2, 5, 3, 6, 7), c(5, 2, 4, 3, 1), sigma = 1.2)
#'
#' #----------------------------------------------------------------------------
#' # Paired-Sample Design
#'
#' # Example 3a: Two-sided paired-sample z-test
#' # population standard deviation of difference score = 1.2
#' test.z(mtcars$drat, mtcars$wt, sigma = 1.2, paired = TRUE)
#'
#' # Example 3b: One-sided paired-sample z-test
#' # population standard deviation of difference score = 1.2
#' # print Cohen's d
#' test.z(mtcars$drat, mtcars$wt, sigma = 1.2, paired = TRUE,
#'        alternative = "greater", effsize = TRUE)
#'
#' # Example 3c: Two-sided paired-sample z-test
#' # population standard deviation of difference score = 1.2
#' # plot results
#' test.z(mtcars$drat, mtcars$wt, sigma = 1.2, paired = TRUE, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Save plot, ggsave() from the ggplot2 package
#' ggsave("Paired-sample_z-test.png", dpi = 600, width = 3, height = 6)
#'
#' # Example 3d: Two-sided paired-sample z-test
#' # population standard deviation of difference score = 1.2
#' # extract plot
#' p <- test.z(mtcars$drat, mtcars$wt, sigma = 1.2, paired = TRUE, output = FALSE)$plot
#' p
#'
#' # Example 1e: Two-sided paired-sample z-test
#' # Draw plot in line with the default setting of test.z()
#'
#' # Extract data
#' plotdat <- data.frame(test.z(mtcars$drat, mtcars$wt, sigma = 1.2, paired = TRUE,
#'                       output = FALSE)$data)
#'
#' # Difference score
#' plotdat$diff <- plotdat$y - plotdat$x
#'
#' # Extract results
#' result <- test.z(mtcars$drat, mtcars$wt, sigma = 1.2, paired = TRUE,
#'                   output = FALSE)$result
#'
#' # Draw plot
#' ggplot(plotdat, aes(0, diff)) +
#'   geom_point(data = result, aes(x = 0, m.diff), size = 4) +
#'   geom_errorbar(data = result,
#'                 aes(x = 0L, y = m.diff, ymin = m.low, ymax = m.upp), width = 0.2) +
#'    scale_x_continuous(name = NULL, limits = c(-2, 2)) +
#'    scale_y_continuous(name = "y") +
#'    geom_hline(yintercept = 0, linetype = 3, linewidth = 0.8) +
#'    labs(subtitle = "Two-Sided 95% Confidence Interval") +
#'    theme_bw() + theme(plot.subtitle = element_text(hjust = 0.5),
#'                       axis.text.x = element_blank(),
#'                       axis.ticks.x = element_blank())
test.z <- function(x, ...) {

  UseMethod("test.z")

}

#_______________________________________________________________________________
#
# Default S3 method ------------------------------------------------------------

test.z.default <- function(x, y = NULL, sigma = NULL, sigma2 = NULL, mu = 0,
                           paired = FALSE, alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95, hypo = TRUE, descript = TRUE, effsize = FALSE,
                           plot = FALSE,  adjust = TRUE, point.size = 4,errorbar.width = 0.1,
                           xlab = NULL, ylab = NULL, ylim = NULL, breaks = ggplot2::waiver(),
                           line = TRUE, linetype = 3, linewidth = 0.8, jitter = TRUE,
                           jitter.size = 1.25, jitter.width = 0.05,
                           jitter.height = 0, jitter.alpha = 0.1,
                           title = "", subtitle = "Confidence Interval",
                           digits = 2, p.digits = 3, as.na = NULL,  write = NULL, append = TRUE,
                           check = TRUE, output = TRUE, ...) {

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
               package = "ggplot2", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

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
                         pval = switch(alternative,
                                       two.sided = pnorm(abs(z), lower.tail = FALSE) * 2,
                                       less = pnorm(z, lower.tail = TRUE),
                                       greater = pnorm(z, lower.tail = FALSE)),
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
                               pval = c(NA, switch(alternative,
                                                   two.sided = pnorm(abs(z), lower.tail = FALSE) * 2L,
                                                   less = pnorm(z, lower.tail = TRUE),
                                                   greater = pnorm(z, lower.tail = FALSE))),
                               d = c(NA, d)), row.names = NULL)

    sample <- "two"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Paired samples ####
  } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

    # Descriptive statistics
    x.ci <- misty::ci.mean.diff(x = x, y = y, sigma = sigma, paired = TRUE,
                                alternative = alternative, output = FALSE)$result

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
                         pval = switch(alternative,
                                       two.sided = pnorm(abs(z), lower.tail = FALSE) * 2L,
                                       less = pnorm(z, lower.tail = TRUE),
                                       greater = pnorm(z, lower.tail = FALSE)),
                         d = d, row.names = NULL)


    sample <- "paired"

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Plot ####

  switch(sample,
         #...................
         ### One-sample ####
         "one" = {

           # Plot data
           plotdat <- data.frame(x = x)

           # Plot Subtitle
           if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0(ifelse(alternative == "two.sided", "Two-Sided ", "One-Sided "),
                                                                               round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

           # Create ggplot
           p <- ggplot2::ggplot(plotdat, ggplot2::aes(x = 0L, y = x))

           # Add jittered points
           if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size) }

           p <- p + ggplot2::geom_point(data = result, ggplot2::aes(x = 0L, m), size = point.size) +
                  ggplot2::geom_errorbar(data = result, ggplot2::aes(x = 0L, y = m, ymin = m.low, ymax = m.upp), width = errorbar.width) +
                  ggplot2::scale_x_continuous(name = xlab, limits = c(-2L, 2L)) +
                  ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = breaks) +
                  ggplot2::labs(title = title, subtitle = subtitle) +
                  ggplot2::theme_bw() + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5),
                                                       plot.subtitle = ggplot2::element_text(hjust = 0.5),
                                                       axis.text.x = ggplot2::element_blank(),
                                                       axis.ticks.x = ggplot2::element_blank())

           # Add horizontal line
           if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = mu, linetype = linetype, linewidth = linewidth) }

           #...................
           ### Two-sample ####
           }, "two" = {

             # Plot data
             plotdat <- data.frame(group = factor(c(rep("x", times = length(x)), rep("y", times = length(y)))), y = c(x, y))

             # Plot Subtitle
             if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0("Two-Sided ", round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

             # Confidence interval
             plot.ci <- data.frame(group = c(1, 2),
                                   rbind(misty::ci.mean(x, sigma = sigma[1L], adjust = adjust, conf.level = conf.level, output = FALSE)$result,
                                         misty::ci.mean(y, sigma = sigma[2L], adjust = adjust, conf.level = conf.level, output = FALSE)$result))

             # Create ggplot
             p <- ggplot2::ggplot(plotdat, ggplot2::aes(group, y))

             # Add jittered points
             if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, size = jitter.size) }

             p <- p + ggplot2::geom_point(data = plot.ci, ggplot2::aes(group, m), stat = "identity", size = point.size) +
                    ggplot2::geom_errorbar(data = plot.ci, ggplot2::aes(group, m, ymin = low, ymax = upp), width = errorbar.width) +
                    ggplot2::scale_x_discrete(name = xlab) +
                    ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = breaks) +
                    ggplot2::theme_bw() +
                    ggplot2::labs(title = title, subtitle = subtitle) +
                    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5), plot.title = ggplot2::element_text(hjust = 0.5))

           #...................
           ### Paired-sample ####
           }, "paired" = {

             # Plot data
             plotdat <- data.frame(x = y - x)

             # Plot Subtitle
             if (isTRUE(subtitle == "Confidence Interval")) { subtitle <- paste0(ifelse(alternative == "two.sided", "Two-Sided ", "One-Sided "),
                                                                                 round(conf.level * 100L, digits = 2L), "% Confidence Interval") }

             # Create ggplot
             p <- ggplot2::ggplot(plotdat, ggplot2::aes(x = 0L, y = x))

             # Add jittered points
             if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, size = jitter.size) }

             p <- p + ggplot2::geom_point(data = result, ggplot2::aes(x = 0L, y = m.diff), size = point.size) +
                    ggplot2::geom_errorbar(data = result, ggplot2::aes(x = 0L, y = m.diff, ymin = m.low, ymax = m.upp), width = errorbar.width) +
                    ggplot2::scale_x_continuous(name = xlab, limits = c(-2, 2)) +
                    ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = breaks) +
                    ggplot2::theme_bw() + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank()) +
                    ggplot2::labs(title = title, subtitle = subtitle) +
                    ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                                   plot.title = ggplot2::element_text(hjust = 0.5))

             # Add horizontal line
             if (isTRUE(line)) { p <- p + ggplot2::geom_hline(yintercept = 0L, linetype = linetype, size = linewidth) }

            })


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print plot ####
  if (isTRUE(plot)) {

    suppressWarnings(print(p))

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------


  object <- list(call = match.call(),
                 type = "test.z",
                 sample = sample,
                 data = list(x = x, y = y),
                 plot = p,
                 args = list(sigma = sigma, sigma2 = sigma2, mu = mu, paired = paired,
                             alternative = alternative, conf.level = conf.level,
                             hypo = hypo, descript = descript, effsize = effsize,
                             plot = plot, point.size = point.size, errorbar.width = errorbar.width,
                             xlab = xlab, ylab = ylab, ylim = ylim, breaks = breaks,
                             line = line, linetype = linetype, linewidth = linewidth,
                             jitter = jitter, jitter.size = jitter.size, jitter.width = jitter.width,
                             jitter.height = jitter.height, jitter.alpha = jitter.alpha,
                             title = title, subtitle = subtitle, digits = digits, p.digits = p.digits,
                             as.na = as.na, write = write, append = append,
                             check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

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
                           plot = FALSE, adjust = TRUE, point.size = 4, errorbar.width = 0.1,
                           xlab = NULL, ylab = NULL, ylim = NULL, breaks = ggplot2::waiver(),
                           jitter = TRUE, jitter.size = 1.25, jitter.width = 0.05,
                           jitter.height = 0, jitter.alpha = 0.1, title = "",
                           subtitle = "Confidence Interval", digits = 2, p.digits = 3,
                           as.na = NULL, write = NULL, append = TRUE, check = TRUE, output = TRUE, ...) {

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
    var.data <- !var.formula %in% colnames(data)
    if (isTRUE(any(var.data))) {

      stop(paste0("Variables specified in the the formula were not found in 'data': ",
                  paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE)

    }

    # Check if input 'formula' has only one grouping variable
    if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

    # Check if input 'formula' has only one outcome variable
    if (isTRUE(length(y.vars) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Global variables
  group <- m <- low <- upp <- adjust <-  NULL

  # Alternative hypothesis
  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  data.split <- split(unlist(data[, y.vars]), f = unlist(data[, group.var]))

  object <- test.z.default(x = data.split[[1L]], y = data.split[[2L]],
                           sigma = sigma, sigma2 = sigma2, alternative = alternative,
                           conf.level = conf.level, hypo = hypo, descript = descript,
                           effsize = effsize, plot = FALSE, point.size = point.size,
                           errorbar.width = errorbar.width, xlab = xlab, ylab = ylab,
                           ylim = ylim, breaks = breaks, jitter = jitter,
                           jitter.size = jitter.size, jitter.width = jitter.width,
                           jitter.height = jitter.height, jitter.alpha = jitter.alpha,
                           title = title, subtitle = subtitle, digits = digits, p.digits = p.digits,
                           as.na = as.na, check = check, output = FALSE)

  object$result[, "group"] <- names(data.split)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Plot ####

  # Label x-axis
  p <- suppressMessages(object$plot + ggplot2::scale_x_discrete(labels = names(data.split)))

  # Print plot
  if (isTRUE(plot)) { suppressWarnings(print(p)) }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "test.z",
                 sample = "two",
                 data = data[, var.formula],
                 args = list(sigma = object$args$sigma, sigma2 = object$args$sigma2,
                             alternative = alternative, conf.level = conf.level,
                             hypo = hypo, descript = descript, effsize = effsize,
                             plot = plot, point.size = point.size, errorbar.width = errorbar.width,
                             xlab = xlab, ylab = ylab, ylim = ylim, breaks = breaks,
                             jitter = jitter, jitter.size = jitter.size, jitter.width = jitter.width,
                             jitter.height = jitter.height, jitter.alpha = jitter.alpha,
                             title = title, subtitle = subtitle, digits = digits, p.digits = p.digits,
                             as.na = as.na, write = write, append = append, check = check, output = object$args$output),
                 formula = object$args$formula,
                 plot = p, result = object$result)

  class(object) <- "misty.object"


  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    # Send R output to textfile
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
