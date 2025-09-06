#' t-Test
#'
#' This function performs one-sample, two-sample, and paired-sample t-tests and
#' provides descriptive statistics, effect size measure, and a plot showing bar
#' plots with error bars for (difference-adjusted) confidence intervals.
#'
#' @param x              a numeric vector of data values.
#' @param y              a numeric vector of data values.
#' @param mu             a numeric value indicating the population mean under the
#'                       null hypothesis. Note that the argument \code{mu} is only
#'                       used when computing a one sample t-test.
#' @param paired         logical: if \code{TRUE}, paired-samples t-test is computed.
#' @param alternative    a character string specifying the alternative hypothesis,
#'                       must be one of \code{"two.sided"} (default),
#'                       \code{"greater"} or \code{"less"}.
#' @param hypo           logical: if \code{TRUE} (default), null and alternative
#'                       hypothesis are shown on the console.
#' @param descript       logical: if \code{TRUE} (default), descriptive statistics
#'                       are shown on the console.
#' @param effsize        logical: if \code{TRUE}, effect size measure Cohen's d is
#'                       shown on the console, see \code{\link{cohens.d}} function.
#' @param weighted       logical: if \code{TRUE}, the weighted pooled standard
#'                       deviation is used to compute Cohen's d for a two-sample
#'                       design (i.e., \code{paired = FALSE}), while standard
#'                       deviation of the difference scores is used to compute
#'                       Cohen's d for a paired-sample design (i.e.,
#'                       \code{paired = TRUE}).
#' @param cor            logical: if \code{TRUE} (default), \code{paired = TRUE},
#'                       and \code{weighted = FALSE}, Cohen's d for a paired-sample
#'                       design while controlling for the correlation between the
#'                       two sets of measurement is computed. Note that this
#'                       argument is only used in
#'                       a paired-sample design (i.e., \code{paired = TRUE}) when
#'                       specifying \code{weighted = FALSE}.
#' @param ref            character string \code{"x"} or \code{"y"} for specifying
#'                       the reference reference group when using the default
#'                       \code{test.t()} function or a numeric value or character
#'                       string indicating the reference group in a two-sample
#'                       design when using the formula \code{test.t()} function.
#'                       The standard deviation of the reference variable or
#'                       reference group is used to standardized the mean difference
#'                       to compute Cohen's d. Note that this argument is only used
#'                       in a two-sample design (i.e., \code{paired = FALSE}).
#' @param correct        logical: if \code{TRUE}, correction factor to remove
#'                       positive bias in small samples is used.
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
#'                       is drawn at \code{mu} for the one-sample t-test or at
#'                       0 for the paired-sample t-test.
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
#' @param formula        in case of two sample t-test (i.e., \code{paired = FALSE}),
#'                       a formula of the form \code{y ~ group} where \code{group}
#'                       is a numeric variable, character variable or factor with
#'                       two values or factor levels giving the corresponding
#'                       groups.
#' @param data           a matrix or data frame containing the variables in the
#'                       formula \code{formula}.
#' @param ...            further arguments to be passed to or from methods.
#'
#' @details
#' \describe{
#' \item{\strong{Effect Size Measure}}{By default, Cohen's d based on the non-weighted
#' standard deviation (i.e., \code{weighted = FALSE}) which does not assume homogeneity
#' of variance is computed (see Delacre et al., 2021) when requesting an effect size
#' measure (i.e., \code{effsize = TRUE}). Cohen's d based on the pooled standard
#' deviation assuming equality of variances between groups can be requested by
#' specifying \code{weighted = TRUE}.}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{aov.b}}, \code{\link{aov.w}}, \code{\link{test.welch}}, \code{\link{test.z}},
#' \code{\link{test.levene}}, \code{\link{cohens.d}}, \code{\link{ci.mean.diff}},
#' \code{\link{ci.mean}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in
#' psychology - Using R and SPSS}. John Wiley & Sons.
#'
#' Delacre, M., Lakens, D., Ley, C., Liu, L., & Leys, C. (2021). Why Hedges' g*s
#' based on the non-pooled standard deviation should be reported with Welch's t-test.
#' https://doi.org/10.31234/osf.io/tu6mp
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
#' # Example 1a: Two-sided one-sample t-test, population mean = 20
#' test.t(mtcars$mpg, mu = 20)
#'
#' # Example 1b: One-sided one-sample t-test, population mean = 20, print Cohen's d
#' test.t(mtcars$mpg, mu = 20, alternative = "greater", effsize = TRUE)
#'
#' # Example 1c: Two-sided one-sample t-test, population mean = 20, plot results
#' test.t(mtcars$mpg, mu = 20, plot = TRUE)
#'
#' # Example 1d: Two-sided one-sample t-test, population mean = 20, save plot
#' test.t(mtcars$mpg, mu = 20, plot = TRUE, filename = "One-sample_t-test.png",
#'        width = 4, height = 5)
#'
#' #----------------------------------------------------------------------------
#' # Two-Sample Design
#'
#' # Example 2a: Two-sided two-sample t-test
#' test.t(mpg ~ vs, data = mtcars)
#'
#' # Example 2b: Two-sided two-sample t-test, alternative specification
#' test.t(c(3, 1, 4, 2, 5, 3, 6, 7), c(5, 2, 4, 3, 1))
#'
#' # Example 2c: One-sided two-sample t-test, print Cohen's d with weighted pooled SD
#' test.t(mpg ~ vs, data = mtcars, alternative = "greater", effsize = TRUE)
#'
#' # Example 2d: Two-sided two-sample t-test, plot results
#' test.t(mpg ~ vs, data = mtcars, plot = TRUE)
#'
#' # Example 2e: Two-sided two-sample t-test, plot results
#' test.t(mpg ~ vs, data = mtcars, plot = TRUE, filename = "Two-sample_t-test.png",
#'        width = 5, height = 6)
#'
#' #----------------------------------------------------------------------------
#' # Paired-Sample Design
#'
#' # Example 3a: Two-sided paired-sample t-test
#' test.t(mtcars$drat, mtcars$wt, paired = TRUE)
#'
#' # Example 3b: One-sided paired-sample t-test,
#' # print Cohen's d based on the SD of the difference scores
#' test.t(mtcars$drat, mtcars$wt, paired = TRUE, alternative = "greater",
#'        effsize = TRUE)
#'
#' # Example 3c: Two-sided paired-sample t-test, plot results
#' test.t(mtcars$drat, mtcars$wt, paired = TRUE, plot = TRUE)
#'
#' # Example 3d: Two-sided paired-sample t-test, save plot
#' test.t(mtcars$drat, mtcars$wt, paired = TRUE, plot = TRUE,
#'        filename = "Paired-sample_t-test.png", width = 4, height = 5)
test.t <- function(x, ...) {

  UseMethod("test.t")

}

#_______________________________________________________________________________
#
# Default S3 method ------------------------------------------------------------

test.t.default <- function(x, y = NULL, mu = 0, paired = FALSE,
                           alternative = c("two.sided", "less", "greater"), conf.level = 0.95,
                           hypo = TRUE, descript = TRUE, effsize = FALSE, weighted = FALSE,
                           cor = TRUE, ref = NULL, correct = FALSE, digits = 2, p.digits = 3,
                           as.na = NULL, plot = FALSE, bar = TRUE, point = FALSE, ci = TRUE,
                           line = TRUE, jitter = FALSE, adjust = TRUE, point.size = 4, errorbar.width = 0.1,
                           xlab = NULL, ylab = NULL, ylim = NULL, ybreaks = ggplot2::waiver(),
                           linetype = 3, linewidth = 0.8, jitter.size = 1.25, jitter.width = 0.05,
                           jitter.height = 0, jitter.alpha = 0.1, title = "",
                           subtitle = "Confidence Interval", filename = NULL,
                           width = NA, height = NA, units = c("in", "cm", "mm", "px"),
                           dpi = 600, write = NULL, append = TRUE, check = TRUE,
                           output = TRUE, ...) {

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) ||is.null(x))) { stop("Please specify a numeric vector for the argument 'x'", call. = FALSE) }

  # Check input 'paired'
  if (isTRUE(!is.logical(paired))) { stop("Please specify TRUE or FALSE for the argument 'paired'.", call. = FALSE) }

  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1L) { stop("More than one variable specified for the argument 'x'.", call. = FALSE) }

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  # Check 'y'
  if (!is.null(y)) {

    # Check if only one variable specified in the input 'y'
    if (ncol(data.frame(y)) != 1L) { stop("More than one variable specified for the argument 'y'.",call. = FALSE) }

    # Convert 'y' into a vector
    y <- unlist(y, use.names = FALSE)

  }

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
    if (isTRUE(nrow(na.omit(data.frame(x = x, y = y))) < 2L)) { stop("After listwise deletion, the number of pairs of observations is less than two.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("hypo", "descript", "effsize", "weighted", "cor", "correct", "plot", "adjust", "line", "jitter", "append", "output"),
               numeric = list(mu = 1L, point.size = 1L, errorbar.width = 1L, ylim = 2L, linewidth = 1L, jitter.size = 1L, jitter.width = 1L, jitter.height = 1L, jitter.alpha = 1L),
               character = list(xlab = 1L, ylab = 1L, title = 1L, subtitle = 1L),
               args = c("alternative", "conf.level", "digits", "p.digits", "write1"),
               envir = environment(), input.check = check)

  # Package ggplot2
  if (isTRUE(check && plot)) { if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed to draw a plot, please install the package.", call. = FALSE) } }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Global variables
  m <- m.low <- m.upp <- group <- low <- upp <- m.diff <- NULL

  #...................
  ### Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One-sample design ####
  if (isTRUE(is.null(y))) {

    # Confidence intervals
    x.ci <- misty::ci.mean.diff(x = x, y = NULL, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)$result

    # Cohen's d
    d <- misty::cohens.d(x = x, y = NULL, paired = FALSE, mu = mu, weighted = FALSE, cor = TRUE, ref = NULL, correct = correct,
                         alternative = alternative, conf.level = conf.level, group = NULL, split = NULL, sort.var = FALSE, check = FALSE, output = FALSE)$result

    # t-test
    t <- t.test(x = x, y = y, mu = mu, alternative = alternative)

    result <- data.frame(n = x.ci[["n"]], nNA = x.ci[["nNA"]], m = x.ci[["m"]], sd = x.ci[["sd"]],
                         m.diff = x.ci[["m"]] - mu, se = t$stderr, m.low = x.ci[["low"]], m.upp = x.ci[["upp"]],
                         t = t$statistic, df = t$parameter, p = t$p.value, d = d$d, d.low = d$low, d.upp = d$upp,
                         row.names = NULL, check.names = FALSE)

    sample <- "one"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two samples design ####
  } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    # Confidence intervals
    x.ci <- misty::df.rename(misty::ci.mean.diff(x = x, y = y, alternative = alternative, conf.level = conf.level, output = FALSE)$result,
                             from = c("between", "low", "upp"), to = c("group", "m.low", "m.upp"))

    # Cohen's d
    d <- misty::cohens.d(x = x, y = y, paired = FALSE, mu = 0L, weighted = weighted,
                         cor = TRUE, ref = ref, correct = correct, alternative = alternative, conf.level = conf.level,
                         group = NULL, split = NULL, sort.var = FALSE, check = FALSE, output = FALSE)$result
    # t-test
    t <- t.test(x = x, y = y, alternative = switch(alternative, two.sided = "two.sided", greater = "less", less = "greater"), var.equal = TRUE)

    result <- data.frame(cbind(x.ci[, c("group", "n", "nNA", "m", "sd", "m.diff")], se = c(NA, t$stderr), x.ci[, c("m.low", "m.upp")],
                               t = c(NA, t$statistic)*-1L, df = c(NA, t$parameter), p = c(NA, t$p.value), d = d$d, d.low = d$low, d.upp = d$upp),
                         row.names = NULL, check.names = FALSE)

    sample <- "two"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Paired samples ####
  } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

    # Confidence intervals
    x.ci <- misty::ci.mean.diff(x = x, y = y, paired = TRUE, alternative = alternative, conf.level = conf.level, output = FALSE)$result

    # Cohen's d
    d <- misty::cohens.d(x = x, y = y, paired = TRUE, mu = 0L, weighted = weighted, cor = cor, ref = ref, correct = correct,
                         alternative = alternative, conf.level = conf.level, group = NULL, split = NULL, sort.var = FALSE, check = FALSE, output = FALSE)$result

    # t-test
    t <- t.test(x = x, y = y, alternative = switch(alternative, two.sided = "two.sided", greater = "less", less = "greater"), paired = TRUE)

    result <- data.frame(n = x.ci[["n"]], nNA = x.ci[["nNA"]], m1 = x.ci[["m1"]], m2 = x.ci[["m2"]],
                         m.diff = x.ci[["m.diff"]], sd.diff = x.ci[["sd.diff"]], se = t$stderr, m.low = x.ci[["low"]], m.upp = x.ci[["upp"]],
                         t = t$statistic*-1, df = t$parameter, p = t$p.value, d = d$d, d.low = d$low, d.upp = d$upp, row.names = NULL, check.names = FALSE)

    sample <- "paired"

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "test.t",
                 sample = sample,
                 data = list(x = x, y = y),
                 args = list(mu = mu, paired = paired, alternative = alternative,
                             conf.level = conf.level, hypo = hypo, descript = descript,
                             effsize = effsize, weighted = weighted, cor = cor, ref = ref,
                             correct = correct, conf.level = conf.level, digits = digits,
                             p.digits = p.digits, as.na = as.na, plot = plot, bar = bar,
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

test.t.formula <- function(formula, data, alternative = c("two.sided", "less", "greater"),
                           conf.level = 0.95, hypo = TRUE, descript = TRUE, effsize = FALSE,
                           weighted = FALSE, cor = TRUE, ref = NULL, correct = FALSE,
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

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) ||is.null(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

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

  #...................
  ### Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    data[, y.vars] <- misty::as.na(data[, y.vars], na = as.na, check = check)

    # Variable with missing values only
    vapply(data[, y.vars, drop = FALSE], function(y) all(is.na(y)), FUN.VALUE = logical(1L)) |>
      (\(y) if (any(y)) {

        stop(paste0("After converting user-missing values into NA, following ", ifelse(sum(y) == 1L, "variable is ", "variables are "), "completely missing: ", paste(names(which(y)), collapse = ", ")), call. = FALSE)

      })()


  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Global variables
  group <- m <- low <- upp <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Alternative hypothesis ####

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Reference group ####

  ref.return <- ref

  # Check if input 'data' is NULL
  if (isTRUE(!is.null(ref))) {

    if (isTRUE(!ref %in% na.omit(unlist(data[, group.var])))) { stop("Reference group specified in the argument 'ref' is not represented in the grouping variable.", call. = FALSE) }

    ifelse(which(unique(sort(na.omit(unlist(data[, group.var])))) %in% ref) == 1, ref <- "x", ref <- "y")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Split data
  data.split <- split(unlist(data[, y.vars]), f = unlist(data[, group.var]))

  # # Default S3 method
  object <- test.t.default(x = data.split[[1L]], y = data.split[[2L]], alternative = alternative,
                           conf.level = conf.level, hypo = hypo, descript = descript, effsize = effsize,
                           weighted = weighted, cor = cor, ref = ref, correct = correct,
                           plot = FALSE, adjust = adjust, point.size = point.size,
                           errorbar.width = errorbar.width, xlab = xlab, ylab = ylab,
                           ylim = ylim, ybreaks = ybreaks, jitter = jitter,
                           jitter.size = jitter.size, jitter.width = jitter.width,
                           jitter.height = jitter.height, jitter.alpha = jitter.alpha, title = title,
                           subtitle = subtitle, check = check, output = FALSE)

  object$result[, "group"] <- names(data.split)

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "test.t",
                 sample = "two",
                 data = data[, var.formula],
                 formula = formula,
                 args = list(alternative = alternative,
                             conf.level = conf.level, hypo = hypo, descript = descript,
                             effsize = effsize, weighted = weighted, cor = cor,
                             ref = ref.return, correct = correct, digits = digits, p.digits = p.digits,
                             as.na = as.na, plot = plot, bar = bar, point = point, ci = ci,
                             line = line, jitter = jitter, adjust = adjust, point.size = point.size,
                             errorbar.width = errorbar.width, xlab = xlab, ylab = ylab, ylim = ylim,
                             ybreaks = ybreaks, linetype = linetype, linewidth = linewidth,
                             jitter.size = jitter.size, jitter.width = jitter.width,
                             jitter.height = jitter.height, jitter.alpha = jitter.alpha,
                             title = title, subtitle = subtitle, filename = filename,
                             width = width, height = height, units = units, dpi = dpi,
                             write = write, append = append, check = check, output = output),
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
