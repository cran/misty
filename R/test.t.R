#' t-Test
#'
#' This function performs one-sample, two-sample, and paired-sample t-tests and
#' provides descriptive statistics, effect size measure, and a plot showing bar
#' plots with error bars for (difference-adjusted) confidence intervals.
#'
#' @param data        a data frame. Note that data are specified either by using
#'                    the argument \code{data} or by using the arguments \code{x}
#'                    and \code{y}.
#' @param ...         an expression indicating the variable names in \code{data},
#'                    e.g., \code{test.t(dat, x1, x2)}. Note that the operators
#'                    \code{+}, \code{-}, \code{~}, \code{:}, \code{::}, and \code{!}
#'                    can also be used to select variables, see 'Details' in the
#'                    \code{\link{df.subset}} function.
#' @param x           a numeric vector of data values.
#' @param y           a numeric vector of data values.
#' @param formula     in case of two sample t-test (i.e., \code{paired = FALSE}),
#'                    a formula of the form \code{y ~ group} where \code{group}
#'                    is a numeric variable, character variable or factor with
#'                    two values or factor levels giving the corresponding
#'                    groups.
#' @param mu          a numeric value indicating the population mean under the
#'                    null hypothesis. Note that the argument \code{mu} is only
#'                    used when computing a one sample t-test.
#' @param paired      logical: if \code{TRUE}, paired-sample t-test is computed.
#' @param alternative a character string specifying the alternative hypothesis,
#'                    must be one of \code{"two.sided"} (default),
#'                    \code{"greater"} or \code{"less"}.
#' @param hypo        logical: if \code{TRUE} (default), null and alternative
#'                    hypothesis are shown on the console.
#' @param descript    logical: if \code{TRUE} (default), descriptive statistics
#'                    are shown on the console.
#' @param effsize     logical: if \code{TRUE}, effect size measure Cohen's d is
#'                    shown on the console, see \code{\link{cohens.d}} function.
#' @param weighted    logical: if \code{TRUE} (default), the weighted pooled
#'                    standard deviation is used to compute Cohen's d for a two-sample
#'                    design (i.e., \code{paired = FALSE}), while standard
#'                    deviation of the difference scores is used to compute
#'                    Cohen's d for a paired-sample design (i.e.,
#'                    \code{paired = TRUE}).
#' @param correct     logical: if \code{TRUE}, correction factor to remove
#'                    positive bias in small samples is used when computing
#'                    Cohen's d.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param p.adj       a character string indicating an adjustment method for multiple
#'                    testing based on \code{\link{p.adjust}}, i.e., \code{none}
#'                    (default), \code{bonferroni}, \code{holm}, \code{hochberg},
#'                    \code{hommel}, \code{BH}, \code{BY}, or \code{fdr}.
#'                    See help page of the \code{\link{p.adjust}} function.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed
#'                    before conducting the analysis (i.e., listwise deletion)
#'                    when specifying more than one outcome variable.
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
#'                    a horizontal line at \code{mu} for the one-sample t-test
#'                    or at \code{0} for the paired-sample t-test.
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
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{aov.b}}, \code{\link{aov.w}}, \code{\link{test.welch}},
#' \code{\link{test.z}}, \code{\link{test.levene}}, \code{\link{cohens.d}},
#' \code{\link{ci.mean.diff}}, \code{\link{ci.mean}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in
#' psychology - Using R and SPSS}. John Wiley & Sons.
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
#' # Example 1a: Two-sided one-sample t-test, population mean = 20
#' test.t(mtcars, mpg, mu = 20)
#'
#' # Example 1b: Alternative specification without using the '...' argument
#' test.t(mtcars$mpg, mu = 20)
#'
#' # Example 1c: One-sided one-sample t-test, population mean = 20, print Cohen's d
#' test.t(mtcars, mpg, mu = 20, alternative = "greater", effsize = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Two-Sample Design
#'
#' # Example 2a: Two-sided two-sample t-test
#' test.t(mpg ~ vs, data = mtcars)
#'
#' # Example 2b: Alternative specification without using the '...' argument
#' test.t(x = df.subset(mtcars, mpg, subset = vs == 0),
#'        y = df.subset(mtcars, mpg, subset = vs == 1))
#'
#' # Example 2c: One-sided two-sample t-test, print Cohen's d
#' test.t(mpg ~ vs, data = mtcars, alternative = "greater", effsize = TRUE)
#'
#' # Example 2d: Multiple two-sample t-tests
#' test.t(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars)
#'
#' # Example 2e: Multiple two-sample t-tests, bonferroni correction
#' test.t(cbind(mpg, cyl, disp, hp, drat, wt) ~ vs, data = mtcars, p.adj = "bonferroni")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Paired-Sample Design
#'
#' # Example 3a: Two-sided paired-sample t-test
#' test.t(mtcars, drat, wt, paired = TRUE)
#'
#' # Example 3b:  Alternative specification without using the '...' argument
#' test.t(mtcars[, c("drat", "wt")], paired = TRUE)
#'
#' # Example 3c: Alternative specification without using the '...' argument
#' test.t(x = mtcars$drat, y = mtcars$wt, paired = TRUE)
#'
#' # Example 3d: One-sided paired-sample t-test, print Cohen's d
#' test.t(mtcars, drat, wt, paired = TRUE, alternative = "greater", effsize = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot
#'
#' # Example 4a: One-Sample Design
#' test.t(mtcars, mpg, mu = 20, plot = TRUE)
#'
#' # Example 4b: Two-Sample Design
#' test.t(mpg ~ vs, data = mtcars, plot = TRUE)
#'
#' # Example 4c: Paired-Sample Design
#' test.t(mtcars, drat, wt, paired = TRUE, plot = TRUE)
#'
#' # Example 4d: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- test.t(mpg ~ vs, data = mtcars)
#' plot(object, geom = c("bar", "ci", "jitter"), jitter.alpha = 0.4, title = "Two-Sample t-Test")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Create Plot Manually
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Example 4a: Two-sample t-test
#' ci.table <- ci.mean(mtcars, mpg, group = "vs", adjust = TRUE, output = FALSE)$result
#'
#' ggplot(ci.table, aes(group, m)) +
#'   geom_bar(aes(group, m), stat = "summary", fun = "mean") +
#'   geom_errorbar(aes(group, m, ymin = low, ymax = upp), width = 0.1) +
#'   theme_bw()
#'
#' # Example 4b: Paired-sample t-test
#' object <- test.t(mtcars, drat, wt, paired = TRUE)
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
#' test.t(mpg ~ vs, data = mtcars, write = "t-Test.txt")
#'
#' # Example 6b:  Write results into an Excel file
#' test.t(mpg ~ vs, data = mtcars, write = "t-Test.xlsx")
#'
#' # Example 6c: Save plot as PNG fine
#' test.t(mpg ~ vs, data = mtcars, plot = TRUE, filename = "t-Test.png",
#'        width = 6, height = 5)
#' }
test.t <- function(...) {

  UseMethod("test.t")

}

#_______________________________________________________________________________
#
# Default S3 method ————————————————————————————————————————————————————————————

test.t.default <- function(data = NULL, ..., x = NULL, y = NULL, mu = 0, paired = FALSE,
                           alternative = c("two.sided", "less", "greater"),
                           hypo = FALSE, descript = TRUE, effsize = FALSE, weighted = TRUE,
                           correct = FALSE, conf.level = 0.95, digits = 2, p.digits = 3,
                           as.na = NULL, plot = FALSE,
                           geom = c("all", "bar", "point", "ci", "line", "jitter"),
                           adjust = TRUE, filename = NULL, width = NA, height = NA,
                           dpi = 600, write = NULL, append = TRUE, check = TRUE,
                           output = TRUE) {

  #_____________________________________________________________________________
  #
  ## Initial Check —————————————————————————————————————————————————————————————

  # Check if input 'data' and 'x' are NULL
  if (isTRUE(is.null(data) && is.null(x))) { stop("Please specify the argument 'data' or the argument 'x'.", call. = FALSE) }

  # Check if input 'data' or 'x' is specified
  if (isTRUE(!is.null(data) && !is.null(x))) { stop("Please specify either the argument 'data' or the argument 'x', but not both.", call. = FALSE) }

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
        assign("x", x[, 1L], envir = parent.frame())

      } else {

        assign("y", NULL, envir = parent.frame())

      }

    })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # One-sample
    if (isTRUE(is.null(y))) {

      # Replace user-specified values with missing values
      x <- .as.na(x, na = as.na)

    # Two- or paired-sample
    } else {

      # Replace user-specified values with missing values
      x <- .as.na(x, na = as.na)
      y <- .as.na(y, na = as.na)

    }

  }

  #_____________________________________________________________________________
  #
  ## Input Check ———————————————————————————————————————————————————————————————

  .check.input(logical = c("paired", "hypo", "descript", "effsize", "weighted", "correct", "adjust", "append", "output"), m.character = list(geom = c("all", "bar", "point", "ci", "line", "jitter")),
               numeric = list(mu = 1L), args = c("xy", "alternative", "conf.level", "digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  ## Arguments —————————————————————————————————————————————————————————————————

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One-Sample Design ####

  if (isTRUE(is.null(y))) {

    # Confidence intervals
    x.ci <- misty::ci.mean.diff(x = x, y = NULL, alternative = alternative, conf.level = conf.level, check = FALSE, output = FALSE)$result

    # Cohen's d
    d <- misty::cohens.d(x = x, y = NULL, paired = FALSE, mu = mu, weighted = FALSE, correct = correct, alternative = alternative, conf.level = conf.level, group = NULL, split = NULL, sort.var = FALSE, check = FALSE, output = FALSE)$result

    # t-test
    t <- t.test(x = x, y = y, mu = mu, alternative = alternative)

    result <- data.frame(n = x.ci[["n"]], nNA = x.ci[["nNA"]], m = x.ci[["m"]], sd = x.ci[["sd"]], m.diff = x.ci[["m"]] - mu, se = t$stderr, m.low = x.ci[["low"]], m.upp = x.ci[["upp"]],
                         t = t$statistic, df = t$parameter, p = t$p.value, d = d$d, d.low = d$low, d.upp = d$upp, row.names = NULL, check.names = FALSE)

    sample <- "one"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two-Sample Design ####

  } else if (isTRUE(!is.null(y) && !isTRUE(paired))) {

    # Confidence intervals
    x.ci <- misty::df.rename(misty::ci.mean.diff(x = x, y = y, alternative = alternative, conf.level = conf.level, output = FALSE)$result, from = c("between", "low", "upp"), to = c("group", "m.low", "m.upp"))

    # Cohen's d
    d <- misty::cohens.d(x = x, y = y, paired = FALSE, mu = 0L, weighted = weighted, correct = correct, alternative = alternative, conf.level = conf.level, group = NULL, split = NULL, sort.var = FALSE, check = FALSE, output = FALSE)$result

    # t-test
    t <- t.test(x = x, y = y, alternative = switch(alternative, two.sided = "two.sided", greater = "less", less = "greater"), var.equal = TRUE)

    result <- data.frame(cbind(x.ci[, c("group", "n", "nNA", "m", "sd", "m.diff")], se = c(NA, t$stderr), x.ci[, c("m.low", "m.upp")], t = c(NA, t$statistic)*-1L, df = c(NA, t$parameter), p = c(NA, t$p.value), d = d$d, d.low = d$low, d.upp = d$upp), row.names = NULL, check.names = FALSE)

    sample <- "two"

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Paired-Sample Design  ####

  } else if (isTRUE(!is.null(y) && isTRUE(paired))) {

    # Confidence intervals
    x.ci <- misty::ci.mean.diff(x = x, y = y, paired = TRUE, alternative = alternative, conf.level = conf.level, output = FALSE)$result

    # Cohen's d
    d <- misty::cohens.d(x = x, y = y, paired = TRUE, mu = 0L, weighted = weighted, cor = TRUE, correct = correct, alternative = alternative, conf.level = conf.level, group = NULL, split = NULL, sort.var = FALSE, check = FALSE, output = FALSE)$result

    # t-test
    t <- t.test(x = x, y = y, alternative = switch(alternative, two.sided = "two.sided", greater = "less", less = "greater"), paired = TRUE)

    result <- data.frame(n = x.ci[["n"]], nNA = x.ci[["nNA"]], m1 = x.ci[["m1"]], m2 = x.ci[["m2"]], m.diff = x.ci[["m.diff"]], sd.diff = x.ci[["sd.diff"]], se = t$stderr, m.low = x.ci[["low"]], m.upp = x.ci[["upp"]],
                         t = t$statistic*-1, df = t$parameter, p = t$p.value, d = d$d, d.low = d$low, d.upp = d$upp, row.names = NULL, check.names = FALSE)

    sample <- "paired"

  }

  #_____________________________________________________________________________
  #
  ## Return Object —————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "test.t",
                 sample = sample,
                 data = list(x = x, y = y),
                 args = list(mu = mu, paired = paired, alternative = alternative, hypo = hypo, descript = descript, effsize = effsize, weighted = weighted, correct = correct, conf.level = conf.level, digits = digits, p.digits = p.digits, as.na = as.na, plot = plot, geom = geom, adjust = adjust, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 plot = NULL, result = result)

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

test.t.formula <- function(formula, data, alternative = c("two.sided", "less", "greater"),
                           hypo = FALSE, descript = TRUE, effsize = FALSE, weighted = TRUE,
                           correct = FALSE, conf.level = 0.95, p.adj = "none",
                           na.omit = FALSE, digits = 2, p.digits = 3, as.na = NULL,
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
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Variables ——————————————————————————————————————————————————————————————————

  # Extract variable names
  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome variables
  y.var <- setdiff(var.formula, group.var)

  #_____________________________________________________________________________
  #
  # Input Check ————————————————————————————————————————————————————————————————

  .check.input(logical = c("hypo", "descript", "effsize", "weighted", "correct", "na.omit", "plot", "adjust", "append", "output"), m.character = list(geom = c("all", "bar", "point", "ci", "jitter")),
               args = c("alternative", "conf.level", "p.adjust", "digits", "p.digits", "write2"), envir = environment(), input.check = check)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Additional Checks ####

  if (isTRUE(check)) {

    # Check if variables are available in the data
    (!var.formula %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(y)], collapse = ", ")), call. = FALSE) })()

    # Check if input 'formula' has only one grouping variable
    if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

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
  ## Listwise Deletion ####

  if (isTRUE(any(is.na(x[, y.var])) && length(y.var) > 1L && na.omit)) {

    apply(x[, y.var], 1L, function(y) all(!is.na(y))) |> (\(p) { x <<- x[p, ]

        warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", sum(!p)), call. = FALSE)

      })()

  }

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

  if (isTRUE(all(c("two.sided", "less", "greater") %in% alternative))) { alternative <- "two.sided" }

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

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One Outcome Variable ####

  if (isTRUE(length(y.var) == 1L)) {

    # Default S3 method
    restab <- split(unlist(x[, y.var]), f = unlist(x[, group.var])) |> (\(p) test.t.default(x = p[[1L]], y = p[[2L]], paired = FALSE, alternative = alternative, conf.level = conf.level, weighted = weighted, correct = correct, check = FALSE, output = FALSE)$result |> (\(q) data.frame(group = names(p), q[, -1L]))())()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## More Than One Outcome Variable ####

  } else {

    # Default S3 method
    restab <- data.frame(variable = rep(y.var, each = 2L), do.call("rbind", lapply(y.var, function(y) misty::test.t(eval(parse(text = paste0(y, " ~ ", group.var))), data = x, alternative = alternative, conf.level = conf.level, weighted = weighted, correct = correct, check = FALSE, output = FALSE)$result)))

    # Multiple-testing adjustment
    if (isTRUE(p.adj != "none")) { restab$p <- p.adjust(restab$p, method = p.adj) }

  }

  #_____________________________________________________________________________
  #
  ## Return Object —————————————————————————————————————————————————————————————

  object <- list(call = match.call(),
                 type = "test.t",
                 sample = "two",
                 data = data[, var.formula],
                 formula = formula,
                 args = list(alternative = alternative, hypo = hypo, descript = descript, effsize = effsize, weighted = weighted, correct = correct, conf.level = conf.level, p.adj = p.adj, digits = digits, p.digits = p.digits, as.na = as.na, plot = plot, geom = geom, adjust = adjust, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
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
