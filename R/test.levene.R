#' Levene's Test for Homogeneity of Variance
#'
#' This function performs Levene's test for homogeneity of variance across two
#' or more independent groups.
#'
#' Levene's test is equivalent to a one-way analysis of variance (ANOVA) with the
#' absolute deviations of observations from the mean of each group as dependent
#' variable (\code{center = "mean"}). Brown and Forsythe (1974) modified the
#' Levene's test by using the absolute deviations of observations from the median
#' (\code{center = "median"}). By default, the Levene's test uses the absolute
#' deviations of observations from the median.
#'
#' @param formula       a formula of the form \code{y ~ group} where \code{y} is
#'                      a numeric variable giving the data values and \code{group}
#'                      a numeric variable, character variable or factor with two
#'                      or more than two values or factor levels giving the
#'                      corresponding groups.
#' @param data          a matrix or data frame containing the variables in the
#'                      formula \code{formula}.
#' @param method        a character string specifying the method to compute the
#'                      center of each group, i.e. \code{method = "median"} (default)
#'                      to compute the Levene's test based on the median (aka
#'                      Brown-Forsythe test) or \code{method = "mean"} to compute
#'                      the Levene's test based on the arithmetic mean.
#' @param as.na         a numeric vector indicating user-defined missing values,
#'                      i.e. these values are converted to \code{NA} before
#'                      conducting the analysis.
#' @param conf.level    a numeric value between 0 and 1 indicating the confidence
#'                      level of the interval.
#' @param hypo          logical: if \code{TRUE} (default), null and alternative
#'                      hypothesis are shown on the console.
#' @param descript      logical: if \code{TRUE} (default), descriptive statistics
#'                      are shown on the console.
#' @param plot          logical: if \code{TRUE}, a plot showing violin plots with
#'                      boxplots is drawn.
#' @param violin.alpha  a numeric value indicating the opacity of the violins.
#' @param violin.trim   logical: if \code{TRUE}, the tails of the violins to the
#'                      range of the data is trimmed.
#' @param box           logical: if \code{TRUE} (default), boxplots are drawn.
#' @param box.alpha     a numeric value indicating the opacity of the boxplots.
#' @param box.width     a numeric value indicating the width of the boxplots.
#' @param jitter        logical: if \code{TRUE} (default), jittered data points
#'                      are drawn.
#' @param jitter.size   a numeric value indicating the \code{size} aesthetic
#'                      for the jittered data points.
#' @param jitter.width  a numeric value indicating the amount of horizontal jitter.
#' @param jitter.height a numeric value indicating the amount of vertical jitter.
#' @param jitter.alpha  a numeric value indicating the opacity of the jittered
#'                      data points.
#' @param gray          logical: if \code{TRUE}, the plot is drawn in gray scale.
#' @param start         a numeric value between 0 and 1, graphical parameter to
#'                      specify the gray value at the low end of the palette.
#' @param end           a numeric value between 0 and 1, graphical parameter to
#'                      specify the gray value at the high end of the palette.
#' @param color         a character vector, indicating the color of the violins
#'                      and the boxes. By default, default ggplot2 colors are
#'                      used.
#' @param xlab          a character string specifying the labels for the x-axis.
#' @param ylab          a character string specifying the labels for the y-axis.
#' @param ylim          a numeric vector of length two specifying limits of the
#'                      limits of the y-axis.
#' @param breaks        a numeric vector specifying the points at which tick-marks
#'                      are drawn at the y-axis.
#' @param title         a character string specifying the text for the title for
#'                      the plot.
#' @param subtitle      a character string specifying the text for the subtitle
#'                      for the plot.
#' @param digits        an integer value indicating the number of decimal places
#'                      to be used for displaying results.
#' @param p.digits      an integer value indicating the number of decimal places
#'                      to be used for displaying the \emph{p}-value.
#' @param write         a character string naming a text file with file extension
#'                      \code{".txt"} (e.g., \code{"Output.txt"}) for writing the
#'                      output into a text file.
#' @param append        logical: if \code{TRUE} (default), output will be appended
#'                      to an existing text file with extension \code{.txt} specified
#'                      in \code{write}, if \code{FALSE} existing text file will be
#'                      overwritten.
#' @param check         logical: if \code{TRUE} (default), argument specification
#'                      is checked.
#' @param output        logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{aov.b}}, \code{\link{test.t}}, \code{\link{test.welch}}
#'
#' @references
#' Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of
#' variances. \emph{Journal of the American  Statistical Association, 69},
#' 364-367.
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{formula} \tab formula (\code{formula}) \cr
#' \code{data} \tab data frame with the outcome and grouping variable \cr
#' \code{plot} \tab ggplot2 object for plotting the results \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list of result table \cr
#' }
#'
#' @export
#'
#' @examples
#' dat <- data.frame(y = c(2, 3, 4, 5, 5, 7, 8, 4, 5, 2, 4, 3),
#'                   group = c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3))
#'
#' # Example 1: Levene's test based on the median with 95% confidence interval
#' test.levene(y ~ group, data = dat)
#'
#' # Example 2: Levene's test based on the arithmetic mean  with 95% confidence interval
#' test.levene(y ~ group, data = dat, method = "mean")
#'
#' # Example 3: Levene's test based on the median with 99% confidence interval
#' test.levene(y ~ group, data = dat, conf.level = 0.99)
#'
#' \dontrun{
#' # Example 4: Write results into a text file
#' test.levene(y ~ group, data = dat, write = "Levene.txt")
#'
#' # Example 5: Levene's test based on the median with 95% confidence interval
#' # plot results
#' test.levene(y ~ group, data = dat, plot = TRUE)
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Save plot, ggsave() from the ggplot2 package
#' ggsave("Levene-test.png", dpi = 600, width = 5, height = 6)
#'
#' # Levene's test based on the median with 95% confidence interval
#' # extract plot
#' p <- test.levene(y ~ group, data = dat, output = FALSE)$plot
#' p
#'
#' # Example 6: Extract data
#' plotdat <- test.levene(y ~ group, data = dat, output = FALSE)$data
#'
#' # Draw violin and boxplots in line with the default setting of test.levene()
#' ggplot(plotdat, aes(group, y, fill = group)) +
#'   geom_violin(alpha = 0.3, trim = FALSE) +
#'   geom_boxplot(alpha = 0.2, width = 0.2) +
#'   geom_jitter(alpha = 0.2, width = 0.05, size = 1.25) +
#'   theme_bw() + guides(fill = "none")
#' }
test.levene <- function(formula, data, method = c("median", "mean"),
                        conf.level = 0.95, hypo = TRUE, descript = TRUE,
                        plot = FALSE, violin.alpha = 0.3, violin.trim = FALSE,
                        box = TRUE, box.alpha = 0.2, box.width = 0.2,
                        jitter = TRUE, jitter.size = 1.25, jitter.width = 0.05,
                        jitter.height = 0, jitter.alpha = 0.2,
                        gray = FALSE, start = 0.9, end = 0.4, color = NULL,
                        xlab = NULL, ylab = NULL, ylim = NULL, breaks = ggplot2::waiver(),
                        title = "", subtitle = "",  digits = 2, p.digits = 3, as.na = NULL,
                        write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a matrix or data frame for the argument 'data'.", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }



  #_____________________________________________________________________________
  #
  # Formula --------------------------------------------------------------------

  #.........................................
  # Variables

  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome
  y.var <- var.formula[-grep(group.var, var.formula)]

  #_____________________________________________________________________________
  #
  # Data frame -----------------------------------------------------------------

  data <- as.data.frame(data, stringsAsFactors = FALSE)

  # Check if variables are in the data
  var.data <- !var.formula %in% colnames(data)
  if (isTRUE(any(var.data))) {

    stop(paste0("Variables specified in the the formula were not found in 'data': ",
                paste(var.formula[which(var.data)], collapse = ", ")), call. = FALSE)

  }

  # Check if input 'formula' has only one grouping variable
  if (isTRUE(length(y.var) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

  # Check if input 'formula' has only one grouping variable
  if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { data[, y.var] <- .as.na(data[, y.var], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  data <- na.omit(data[, var.formula])

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  if (isTRUE(check)) {

    # ggplot2 package
    if (isTRUE(plot && !nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed for drawing a bar chart, please install the package.", call. = FALSE) }

    # Variance zero
    y.var0 <- tapply(data[, y.var], data[, group.var], var, na.rm = TRUE) == 0L

    if (isTRUE(any(y.var0))) { stop(paste0("There are groups with 0 variance: Group ", paste(which(y.var0), collapse = ", ")), call. = FALSE) }

    # Check input 'method'
    if (isTRUE(!all(method %in% c("median", "mean")))) { stop("Character string in the argument 'method' does not match with \"median\", or \"mean\".", call. = FALSE) }

    # Check input 'conf.level'
    if (isTRUE(conf.level >= 1L|| conf.level <= 0L)) { stop("Please specifiy a numeric value between 0 and 1 for the argument 'conf.level'.", call. = FALSE) }

    # Check input 'hypo'
    if (isTRUE(!is.logical(hypo))) { stop("Please specify TRUE or FALSE for the argument 'hypo'.", call. = FALSE) }

    # Check input 'descript'
    if (isTRUE(!is.logical(descript))) { stop("Please specify TRUE or FALSE for the argument 'descript'.", call. = FALSE) }

    # Check input 'plot'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'plot'.", call. = FALSE) }

    # Check input 'violin.trim '
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'violin.trim '.", call. = FALSE) }

    # Check input 'box'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'box'.", call. = FALSE) }

    # Check input 'jitter'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'jitter'.", call. = FALSE) }

    # Check input 'gray'
    if (isTRUE(!is.logical(gray))) { stop("Please specify TRUE or FALSE for the argument 'gray'", call. = FALSE) }

    # Check input 'start'
    if (isTRUE(start < 0L || start > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'start'", call. = FALSE) }

    # Check input 'end'
    if (isTRUE(end < 0L || end > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'end'", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1 != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'p.digits'
    if (isTRUE(p.digits %% 1 != 0L || p.digits < 0L)) { stop("Specify a positive integer number for the argument 'p.digits'.", call. = FALSE) }

    # Check input 'write'
    if (isTRUE(!is.null(write) && substr(write, nchar(write) - 3L, nchar(write)) != ".txt")) { stop("Please specify a character string with file extenstion '.txt' for the argument 'write'.") }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Method ####

  if (isTRUE(all(c("median", "mean") %in% method))) { method <- "median" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Outcome
  y <- data[, y.var]

  # Grouping variable
  group <- data[, group.var]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confidence interval ####

  result.ci <- misty::ci.var(y, group = group, conf.level = conf.level, output = FALSE)$result

  result.ci <- data.frame(result.ci[, c("group", "variable", "n", "nNA", "m")],
                          sd = sqrt(result.ci$var),
                          result.ci[, c("var", "low", "upp")])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Analysis of Variance ####

  #...................
  ### Brown-Forsythe test ####
  if (isTRUE(method == "median")) {

    # Median by grouping variable
    y.center <- tapply(y, group, median)

  #...................
  ### Levene's test ####
  } else if (isTRUE(method == "mean")) {

    # Mean by grouping variable
    y.center <- tapply(y, group, mean)

  }

  # Deviation from the median or mean
  y.dev <- abs(y - y.center[ group])

  # Analysis of Variance
  result.aov <- summary(aov(y.dev ~ as.factor(group)))[[1L]]

  row.names(result.aov) <- c("Group", "Residuals")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Plot ####

  #...................
  ### Plot data ####

  plotdat <- data.frame(group = factor(group), y = y, row.names = NULL)

  #...................
  ### Create plot ####

  p <- ggplot2::ggplot(plotdat, ggplot2::aes(group, y, fill = group)) +
         ggplot2::geom_violin(alpha = violin.alpha, trim = violin.trim) +
         ggplot2::scale_x_discrete(name = xlab) +
         ggplot2::scale_y_continuous(name = ylab, limits = ylim, breaks = breaks) +
         ggplot2::theme_bw() +
         ggplot2::labs(title = title, subtitle = subtitle) +
         ggplot2::theme(plot.subtitle = ggplot2::element_text(hjust = 0.5),
                        plot.title = ggplot2::element_text(hjust = 0.5)) +
         ggplot2::guides(fill = "none")

  # Add boxplots
  if (isTRUE(box)) { p <- p + ggplot2::geom_boxplot(alpha = box.alpha, width = box.width) }

  # Add jittered points
  if (isTRUE(jitter)) { p <- p + ggplot2::geom_jitter(alpha = jitter.alpha, width = jitter.width, height = jitter.height, size = jitter.size) }

  # Gray color scales
  if (isTRUE(gray)) {

    p <- p + ggplot2::scale_fill_grey(start = start, end = end)

  # User-specified colors
  } else {

    if (isTRUE(!is.null(color))) { p <- p + ggplot2::scale_fill_manual(values = color) }

  }

  #......................................
  # Print plot
  if (isTRUE(plot)) { suppressWarnings(print(p)) }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "test.levene",
                 formula = formula,
                 data = data.frame(y, group = factor(group), stringsAsFactors = FALSE),
                 plot = p,
                 args = list(method = method, conf.level = conf.level, hypo = hypo,
                             descript = descript, plot = plot, violin.alpha = violin.alpha,
                             violin.trim = violin.trim, box = box, box.alpha = box.alpha,
                             box.width = box.width, jitter = jitter, jitter.size = jitter.size,
                             jitter.width = jitter.width, jitter.height = jitter.height ,
                             jitter.alpha = jitter.alpha, gray = gray, start = start,
                             end = end, color = color, xlab = xlab, ylab = ylab,
                             ylim = ylim, breaks = breaks, title = title, subtitle = subtitle,
                             digits = digits, p.digits = p.digits, as.na = as.na,
                             write = write, append = append, check = check, output = output),
                 result = list(descript = result.ci, test = result.aov))

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
