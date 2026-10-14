#' z-Test
#'
#' This function performs the one-sample, two-sample, and paired-sample z-test
#' and provides descriptive statistics, effect size measure, and a plot showing
#' error bars for (difference-adjusted) confidence intervals with jittered data
#' points.
#'
#' @param data        a data frame. Note that data are specified either by using
#'                    the argument \code{data} or by using the arguments \code{x}
#'                    and \code{y}.
#' @param ...         an expression indicating the variable names in \code{data},
#'                    e.g., \code{test.z(dat, x1, sigma = 2)}. Note that the operators
#'                    \code{+}, \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
#'                    can also be used to select variables, see 'Details' in the
#'                    \code{\link{df.subset}} function.
#' @param x           a numeric vector of data values.
#' @param y           a numeric vector of data values.
#' @param formula     in case of two sample z-test (i.e., \code{paired = FALSE}),
#'                    a formula of the form \code{y ~ group} where \code{group}
#'                    is a numeric variable, character variable or factor with
#'                    two values or factor levels giving the corresponding
#'                    groups.
#' @param sigma       a numeric vector indicating the population standard deviation(s).
#'                    In case of two-sample z-test, equal standard deviations are
#'                    assumed when specifying one value for the argument \code{sigma};
#'                    when specifying two values for the argument \code{sigma},
#'                    unequal standard deviations are assumed. Note that either
#'                    argument \code{sigma} or argument \code{sigma2} is specified.
#' @param sigma2      a numeric vector indicating the population variance(s). In
#'                    case of two-sample z-test, equal variances are assumed when
#'                    specifying one value for the argument \code{sigma2}; when
#'                    specifying two values for the argument \code{sigma}, unequal
#'                    variance are assumed. Note that either argument \code{sigma}
#'                    or argument \code{sigma2} is specified.
#' @param mu          a numeric value indicating the population mean under the null
#'                    hypothesis. Note that the argument \code{mu} is only used
#'                    when computing a one-sample z-test.
#' @param paired      logical: if \code{TRUE}, paired-sample z-test is computed.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default), \code{"greater"}
#'                    or \code{"less"}.
#' @param hypo        logical: if \code{TRUE} (default), null and alternative
#'                    hypothesis are shown on the console.
#' @param descript    logical: if \code{TRUE} (default), descriptive statistics
#'                    are shown on the console.
#' @param effsize     logical: if \code{TRUE}, effect size measure Cohen's d is
#'                    shown on the console.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param digits      an integer value indicating the number of decimal places
#'                    to be used for displaying descriptive statistics and
#'                    confidence interval.
#' @param p.digits    an integer value indicating the number of decimal places
#'                    to be used for displaying the \emph{p}-value.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before
#'                    conducting the analysis.
#' @param plot        logical: if \code{TRUE}, a plot showing bar plots with
#'                    error bars for confidence intervals is drawn. For
#'                    additional plotting arguments, see \code{Details} in
#'                    the help page of the function \code{plot.misty.object}.
#' @param geom        a character string or character vector indicating which
#'                    plot elements to draw, i.e.,  \code{"all"} for drawing
#'                    all plot elements, \code{"bar"} for drawing bars representing
#'                    means for each groups, \code{"point"} for drawing points
#'                    representing means for each group, \code{"ci"} for drawing
#'                    error bars representing confidence intervals, \code{"jitter"}
#'                    for drawing jittered data points, \code{"line"} for drawing
#'                    a horizontal line at \code{mu} for the one-sample z-test
#'                    or at \code{0} for the paired-sample z-test.
#' @param adjust      logical: if \code{TRUE} (default), difference-adjustment
#'                    for the confidence intervals in a two-sample design is
#'                    applied.
#' @param filename    a character string indicating the \code{filename}
#'                    argument including the file extension in the \code{ggsave}
#'                    function. Note that one of \code{".eps"}, \code{".ps"},
#'                    \code{".tex"}, \code{".pdf"} (default),
#'                    \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                    \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                    to be specified as file extension in the \code{file}
#'                    argument. Note that plots can only be saved when
#'                    \code{plot = TRUE}.
#' @param width       a numeric value indicating the \code{width} argument
#'                    (default is the size of the current graphics device)
#'                    for the \code{ggsave} function.
#' @param height      a numeric value indicating the \code{height} argument
#'                    (default is the size of the current graphics device)
#'                    for the \code{ggsave} function.
#' @param dpi         a numeric value indicating the \code{dpi} argument
#'                    (default is \code{600}) for the \code{ggsave} function.
#' @param write       a character string naming a text file with file extension
#'                    \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                    output into a text file.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will be
#'                    overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown on the
#'                    console.
#'
#' @details
#' \describe{
#' \item{\strong{Effect Size}}{The Cohen's d reported when the argument \code{effsize}
#' is set to \code{TRUE} is based on the population standard deviation specified
#' in the argument \code{sigma} or the square root of the population variance specified
#' in the argument \code{sigma2}.
#'    \itemize{
#'      \item{\strong{One-Sample and Paired-Sample Design}} In a one-sample and paired-sample
#'      design, Cohen's d is the mean of the difference scores divided by the population
#'      standard deviation of the (difference) scores equivalent to Cohen's \eqn{d_z}
#'      (Lakens, 2013).
#'      \item{\strong{Two-Sample Design}} In a two-sample design, Cohen's d is the difference
#'      between means of the two groups of observations divided by either the population
#'      standard deviation when assuming and specifying equal standard deviations or
#'      the unweighted pooled population standard deviation when assuming and specifying
#'      unequal standard deviations.
#'    }
#' }
#' }
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
#' #————————————————————————————————————————————————————————————————————————————
#' # One-Sample Design
#'
#' # Example 1a: Two-sided one-sample z-test, population mean = 20, population SD = 6
#' test.z(mtcars, mpg, sigma = 6, mu = 20)
#'
#' # Example 1b: Alternative specification without using the '...' argument
#' test.z(mtcars$mpg, sigma = 6, mu = 20)
#'
#' # Example 1c: One-sided one-sample z-test, population mean = 20, population SD = 6,
#' # print Cohen's d
#' test.z(mtcars$mpg, sigma = 6, mu = 20, alternative = "greater", effsize = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Two-Sample Design
#'
#' # Example 2a: Two-sided two-sample z-test, population SD = 6, equal SD assumption
#' test.z(mpg ~ vs, data = mtcars, sigma = 6)
#'
#' # Example 2b: Alternative specification without using the '...' argument
#' test.t(x = df.subset(mtcars, mpg, subset = vs == 0),
#'        y = df.subset(mtcars, mpg, subset = vs == 1), sigma = 6)
#'
#' # Example 2c: Two-sided two-sample z-test, population SD = 4 and 6, unequal SD assumption
#' test.z(mpg ~ vs, data = mtcars, sigma = c(4, 6))
#'
#' # Example 2d: One-sided two-sample z-test, population SD = 4 and 6, unequal SD assumption
#' # print Cohen's d
#' test.z(mpg ~ vs, data = mtcars, sigma = c(4, 6), alternative = "greater", effsize = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Paired-Sample Design
#'
#' # Example 3a: Two-sided paired-sample z-test, population SD of difference score = 1.2
#' test.z(mtcars, drat, wt, sigma = 1.2, paired = TRUE)
#'
#' # Example 3b: Alternative specification without using the '...' argument
#' test.z(mtcars[, c("drat", "wt")], sigma = 1.2, paired = TRUE)
#'
#' # Example 3c: Alternative specification without using the '...' argument
#' test.z(x = mtcars$drat, y = mtcars$wt, sigma = 1.2, paired = TRUE)
#'
#' # Example 3d: One-sided paired-sample z-test, population SD of difference score = 1.2,
#' # print Cohen's d
#' test.z(x = mtcars$drat, y = mtcars$wt, sigma = 1.2, paired = TRUE, alternative = "greater",
#'        effsize = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot
#'
#' # Example 4a: One-sample design
#' test.z(mtcars, mpg, sigma = 6, mu = 20, plot = TRUE)
#'
#' # Example 4b: Two-sample design
#' test.z(mpg ~ vs, data = mtcars, sigma = 6, plot = TRUE)
#'
#' # Example 4c: Paired-sample design
#' test.z(mtcars, drat, wt, sigma = 1.2, paired = TRUE, plot = TRUE)
#'
#' # Example 4d: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- test.z(mpg ~ vs, data = mtcars, sigma = 6)
#' plot(object, geom = c("bar", "ci", "jitter"), jitter.alpha = 0.4, title = "Two-Sample z-Test")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Create Plot Manually
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Example 4a: Two-sample z-test
#' ci.table <- ci.mean(mtcars, mpg, group = "vs", adjust = TRUE, output = FALSE)$result
#'
#' ggplot(ci.table, aes(group, m)) +
#'   geom_bar(aes(group, m), stat = "summary", fun = "mean") +
#'   geom_errorbar(aes(group, m, ymin = low, ymax = upp), width = 0.1) +
#'   theme_bw()
#'
#' # Example 4b: Paired-sample z-test
#' object <- test.z(mtcars, drat, wt, sigma = 1.2, paired = TRUE)
#'
#' ggplot(data.frame(x = object$data$y - object$data$x), aes(x = 0L, y = x)) +
#'  geom_bar(data = object$result, aes(0, m.diff), stat = "summary", fun = "mean") +
#'  geom_errorbar(data = object$result, aes(0, m.diff, ymin = m.low, ymax = m.upp), width = 0.1) +
#'  geom_hline(yintercept = 0L, linetype = 3, linewidth = 0.8) +
#'  scale_x_continuous(name = "", limits = c(-2, 2)) +
#'  theme_bw()  +
#'  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
#'
#' \dontrun{
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results and Save Plot
#'
#' # Example 6a: Write results into a text file
#' test.z(mpg ~ vs, data = mtcars, sigma = 6, write = "z-Test.txt")
#'
#' # Example 6b:  Write results into an Excel file
#' test.z(mpg ~ vs, data = mtcars, sigma = 6, write = "z-Test.xlsx")
#'
#' # Example 4c: Two-Sample Design
#' test.z(mpg ~ vs, data = mtcars, sigma = 6, plot = TRUE, filename = "z-Test.png",
#'        width = 6, height = 5)
#' }
test.z <- function( ...) {

  UseMethod("test.z")

}

#_______________________________________________________________________________
#
# Default S3 method ————————————————————————————————————————————————————————————

test.z.default <- function(data = NULL, ..., x = NULL, y = NULL, sigma = NULL, sigma2 = NULL, mu = 0,
                           paired = FALSE, alternative = c("two.sided", "less", "greater"),
                           hypo = FALSE, descript = TRUE, effsize = FALSE, conf.level = 0.95,
                           digits = 2, p.digits = 3, as.na = NULL, plot = FALSE,
                           geom = c("all", "bar", "point", "ci", "line", "jitter"),
                           adjust = TRUE, filename = NULL, width = NA, height = NA,
                           dpi = 600, write = NULL, append = TRUE, check = TRUE,
                           output = TRUE) {

  #_____________________________________________________________________________
  #
  ## Initial Check ####

  # Check if input 'data' and 'x' are NULL
  if (isTRUE(is.null(data) && is.null(x))) { stop("Please specify the argument 'data' or the argument 'x'.", call. = FALSE) }

  # Check if input 'data' or 'x' is specified
  if (isTRUE(!is.null(data) && !is.null(x))) { stop("Please specify either the argument 'data' or the argument 'x', but not both.", call. = FALSE) }

  # Check input 'sigma'
  if (isTRUE(is.null(sigma) && is.null(sigma2))) { stop("Please specify either argument 'sigma' or argument 'sigma2'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data ———————————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Extract Variables ####

  if (isTRUE(!is.null(data))) {

    .data(data, ..., drop = TRUE, check.one.two = TRUE) |> (\(p) {

      # Extract 'x'
      assign("x", p$data, envir = parent.frame())

      # Extract 'y'
      if (isTRUE(ncol(as.data.frame(p$data)) > 1L)) {

        assign("y", x[, 2L], envir = parent.frame())
        assign("x", x[, 1L] , envir = parent.frame())

      } else {

        assign("y", NULL, envir = parent.frame())

      }

    })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

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

  #_____________________________________________________________________________
  #
  ## Input Check ———————————————————————————————————————————————————————————————

  .check.input(logical = c("paired", "hypo", "descript", "effsize", "adjust", "append", "output"), m.character = list(geom = c("all", "bar", "point", "ci", "line", "jitter")),
               numeric = list(mu = 1L), args = c("xy", "alternative", "conf.level", "digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    #—————————————————————————————————————— #
    ### Check input 'sigma' and 'sigma2' ####

    if (isTRUE(!is.null(sigma) && !is.null(sigma2))) { if (isTRUE(!identical(sigma^2, sigma2))) { stop("Arguments 'sigma' and 'sigma2' do not match.", call. = FALSE) } }

    #—————————————————————————————————————— #
    ### Check input 'sigma' ####

    # Check input 'sigma'
    if (isTRUE(!is.null(sigma))) {

      # SD smaller or equal 0
      if (isTRUE(any(sigma <= 0L))) { stop("Please specify numeric values grater than 0 for the argument 'sigma'.", call. = FALSE) }

      # One sample
      if (isTRUE(is.null(y))) {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 1L)) { stop("Please specify one numeric values for the argument 'sigma' for one sample.", call. = FALSE) }

      # Two sample
      } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 2L)) { stop("Please specify one or two numeric values for the argument 'sigma' in independent samples.", call. = FALSE) }

      # Paired sample
      } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

        # Length of 'sigma'
        if (isTRUE(length(sigma) > 1L)) { stop("Please specify one numeric values for the argument 'sigma' in paired samples.", call. = FALSE) }

      }

    }

    #—————————————————————————————————————— #
    ### Check input 'sigma2' ####

    # Check input 'sigma2'
    if (isTRUE(!is.null(sigma2))) {

      # Variance smaller or equal 0
      if (isTRUE(any(sigma2 <= 0L))) { stop("Please specify numeric values grater than 0 for the argument 'sigma2'.", call. = FALSE) }

      if (!isTRUE(paired)) {

        # Length of 'sigma2'
        if (length(sigma2) > 2L) { stop("Please specify one or two numeric values for the argument 'sigma2' in paired samples.", call. = FALSE) }

      } else {

        # Length of 'sigma2'
        if (isTRUE(length(sigma2) > 1L)) { stop("Please specify one numeric values for the argument 'sigma2' in paired samples.", call. = FALSE) }

      }

    }

  }

  #_____________________________________________________________________________
  #
  ## Arguments —————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Population Standard Deviation and Variance ####

  #—————————————————————————————————————— #
  ### One-Sample Design ####

  if (isTRUE(is.null(sigma) && !is.null(sigma2))) { sigma <- sqrt(sigma2) }

  if (isTRUE(!is.null(sigma) && is.null(sigma2))) { sigma2 <- sigma^2 }

  #—————————————————————————————————————— #
  ### Two-Sample Design ####

  if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    if (isTRUE(!is.null(sigma) && length(sigma) == 1L)) { sigma <- c(sigma, sigma) }

    if (isTRUE(!is.null(sigma2) && length(sigma2) == 1L)) { sigma2 <- c(sigma2, sigma2) }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'geom' Argument ####

  if (isTRUE(all(c("all", "bar", "point", "ci", "line", "jitter") %in% geom))) {

    geom <- c("bar", "ci", "line")

  } else if (all(geom == "all")) {

    geom <- c("bar", "point", "ci", "line", "jitter")

  }

  #_____________________________________________________________________________
  #
  ## Main Function —————————————————————————————————————————————————————————————

  #—————————————————————————————————————— #
  ### One-Sample Design ####

  if (isTRUE(is.null(y))) {

    # Descriptive statistics
    x.ci <- misty::ci.mean(x, sigma = sigma, alternative = alternative, output = FALSE)$result

    # Standard error of the mean
    se <- (sigma / sqrt(x.ci[["n"]]))

    # Test statistic
    z <- (x.ci[["m"]] - mu) / se

    # Cohen's d
    d <- (x.ci[["m"]] - mu) / sigma

    result <- data.frame(n = x.ci[["n"]], nNA = x.ci[["nNA"]], m = x.ci[["m"]], sd = x.ci[["sd"]], m.diff = x.ci[["m"]] - mu, se = se, m.low = x.ci[["low"]], m.upp = x.ci[["upp"]], z = z,
                         p = switch(alternative, two.sided = pnorm(abs(z), lower.tail = FALSE) * 2L, less = pnorm(z, lower.tail = TRUE), greater = pnorm(z, lower.tail = FALSE)),
                         d = d, row.names = NULL)

    sample <- "one"

  #—————————————————————————————————————— #
  ### Two-Sample Design ####

  } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    # Descriptive statistics
    x.ci <- misty::df.rename(misty::ci.mean.diff(x = x, y = y, sigma = sigma, alternative = alternative, output = FALSE)$result, from = c("between", "low", "upp"), to = c("group", "m.low", "m.upp"))

    # Standard error of the mean difference
    se <- sqrt((sigma2[1L] / x.ci[1L, "n"]) + (sigma2[2L] / x.ci[2L, "n"]))

    # Test statistic
    z <- x.ci[2L, "m.diff"] / se

    # Cohen's d
    d <- x.ci[2L, "m.diff"] / mean(sigma)

    result <- data.frame(cbind(x.ci[, c("group", "n", "nNA", "m", "sd", "m.diff")], se = c(NA, se), x.ci[, c("m.low", "m.upp")], z = c(NA, z),
                               p = c(NA, switch(alternative, two.sided = pnorm(abs(z), lower.tail = FALSE) * 2L, less = pnorm(z, lower.tail = TRUE), greater = pnorm(z, lower.tail = FALSE))),
                               d = c(NA, d)), row.names = NULL)

    sample <- "two"

  #—————————————————————————————————————— #
  ### Paired-Sample Design ####

  } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

    # Descriptive statistics
    x.ci <- misty::ci.mean.diff(x = x, y = y, sigma = sigma, paired = TRUE, alternative = alternative, output = FALSE)$result

    # Standard error of the mean difference
    se <- (sigma / sqrt(x.ci[["n"]]))

    # Test statistic
    z <- (x.ci[["m.diff"]]) / se

    # Cohen's d
    d <- (x.ci[["m.diff"]]) / sigma

    result <- data.frame(n  = x.ci[["n"]], nNA = x.ci[["nNA"]], m1 = x.ci[["m1"]], m2 = x.ci[["m2"]], m.diff = x.ci[["m.diff"]], sd.diff = x.ci[["sd.diff"]],
                         se = se, m.low = x.ci[["low"]], m.upp = x.ci[["upp"]], z = z, p = switch(alternative, two.sided = pnorm(abs(z), lower.tail = FALSE) * 2L, less = pnorm(z, lower.tail = TRUE), greater = pnorm(z, lower.tail = FALSE)), d = d, row.names = NULL)

    sample <- "paired"

  }

  #_____________________________________________________________________________
  #
  ## Return Object —————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "test.z",
                 sample = sample,
                 data = list(x = x, y = y),
                 args = list(sigma = sigma, sigma2 = sigma2, mu = mu, paired = paired, alternative = alternative, hypo = hypo, descript = descript, effsize = effsize, conf.level = conf.level, digits = 2, p.digits = 3, as.na = NULL, plot = plot, geom = geom, adjust = adjust, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 plot = NULL,
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  ## Plot and Save Results —————————————————————————————————————————————————————

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  ## Write Results —————————————————————————————————————————————————————————————

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  ## Output ————————————————————————————————————————————————————————————————————

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
#
# S3 method for class 'formula' ————————————————————————————————————————————————

test.z.formula <- function(formula, data, sigma = NULL, sigma2 = NULL,
                           alternative = c("two.sided", "less", "greater"),
                           hypo = FALSE, descript = TRUE, effsize = FALSE,
                           conf.level = 0.95, digits = 2, p.digits = 3, as.na = NULL,
                           plot = FALSE, geom = c("all", "bar", "point", "ci", "jitter"),
                           adjust = TRUE, filename = NULL, width = NA, height = NA,
                           dpi = 600, write = NULL, append = TRUE, check = TRUE,
                           output = TRUE, ...) {

  #_____________________________________________________________________________
  #
  ## Initial Check —————————————————————————————————————————————————————————————

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) ||is.null(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Variables ——————————————————————————————————————————————————————————————————

  # Extract variable names
  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome variable
  y.var <- setdiff(var.formula, group.var)

  #_____________________________________________________________________________
  #
  # Input Check ————————————————————————————————————————————————————————————————

  .check.input(logical = c("hypo", "descript", "effsize", "plot", "adjust", "append", "output"), m.character = list(geom = c("all", "bar", "point", "ci", "jitter")),
               args = c("alternative", "conf.level", "digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    # Check if variables are in the data
    (!var.formula %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(y)], collapse = ", ")), call. = FALSE) })()

    # Check if input 'formula' has only one grouping variable
    if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

    # Check if input 'formula' has only one outcome variable
    if (isTRUE(length(y.var) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data ———————————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame ####

  x <- as.data.frame(data)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { x[, y.var] <- .as.na(x[, y.var], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data in the Grouping Variable ####

  if (isTRUE(any(is.na(x[, group.var])))) {

    (!is.na(x[, group.var])) |> (\(p) { x <<- x[p, ]

    warning(paste0("Listwise deletion of missing values in the grouping variable, number of cases removed from the analysis: ", sum(!p)), call. = FALSE)

    })()

  }

  #_____________________________________________________________________________
  #
  ## Arguments —————————————————————————————————————————————————————————————————

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'alternative' Argument ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { altrnative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'geom' Argument ####

  if (isTRUE(all(c("all", "bar", "point", "ci", "jitter") %in% geom))) {

    geom <- c("bar", "ci")

  } else if (all(geom == "all")) {

    geom <- c("bar", "point", "ci", "jitter")

  }
  #_____________________________________________________________________________
  #
  ## Main Function —————————————————————————————————————————————————————————————

  # Default S3 method
  restab <- split(unlist(x[, y.var]), f = unlist(x[, group.var])) |> (\(p) test.z.default(x = p[[1L]], y = p[[2L]], paired = FALSE, sigma = sigma, sigma2 = sigma2, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)$result |> (\(q) data.frame(group = names(p), q[, -1L]))())()

  #_____________________________________________________________________________
  #
  ## Return Object —————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "test.z",
                 sample = "two",
                 data = data[, var.formula],
                 formula = formula,
                 args = list(sigma = sigma, sigma2 = sigma2, alternative = alternative,  hypo = hypo, descript = descript, effsize = effsize, conf.level = conf.level, digits = digits, p.digits = p.digits, as.na = as.na, plot = plot, geom = geom, adjust = adjust, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 plot = NULL,
                 result = restab)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  ## Plot and Save Plot ————————————————————————————————————————————————————————

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  ## Write Results —————————————————————————————————————————————————————————————

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  ## Output ————————————————————————————————————————————————————————————————————

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
