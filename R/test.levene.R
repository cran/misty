#' Levene's Test for Homogeneity of Variance
#'
#' This function performs Levene's test for homogeneity of variance across two
#' or more independent groups including a plot showing violin plots and boxplots
#' representing the distribution of the outcome variable for each group.
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
#' @param conf.level    a numeric value between 0 and 1 indicating the confidence
#'                      level of the interval.
#' @param hypo          logical: if \code{TRUE} (default), null and alternative
#'                      hypothesis are shown on the console.
#' @param descript      logical: if \code{TRUE} (default), descriptive statistics
#'                      are shown on the console.
#' @param digits        an integer value indicating the number of decimal places
#'                      to be used for displaying results.
#' @param p.digits      an integer value indicating the number of decimal places
#'                      to be used for displaying the \emph{p}-value.
#' @param as.na         a numeric vector indicating user-defined missing values,
#'                      i.e. these values are converted to \code{NA} before
#'                      conducting the analysis.
#' @param plot          logical: if \code{TRUE}, a plot showing violins with
#'                      boxplots is drawn.
#' @param violin        logical: if \code{TRUE} (default), violins are drawn.
#' @param box           logical: if \code{TRUE} (default), boxplots are drawn.
#' @param jitter        logical: if \code{TRUE} (default), jittered data points
#'                      are drawn.
#' @param violin.alpha  a numeric value between 0 and 1 for specifying the
#'                      \code{alpha} argument in the \code{geom_violin}
#'                      function for controlling the opacity of the violins.
#' @param violin.trim   logical: if \code{TRUE}, the tails of the violins to the
#'                      range of the data is trimmed.
#' @param box.alpha     a numeric value between 0 and 1 for specifying the
#'                      \code{alpha} argument in the \code{geom_boxplot}
#'                      function for controlling the opacity of the boxplots.
#' @param box.width     a numeric value indicating the width of the boxplots.
#' @param jitter.size   a numeric value indicating the \code{size} aesthetic
#'                      for the jittered data points.
#' @param jitter.width  a numeric value indicating the amount of horizontal jitter.
#' @param jitter.height a numeric value indicating the amount of vertical jitter.
#' @param jitter.alpha  a numeric value between 0 and 1 for specifying the
#'                      \code{alpha} argument in the \code{geom_jitter}
#'                      function for controlling the opacity of the jittered
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
#' @param ybreaks       a numeric vector specifying the points at which tick-marks
#'                      are drawn at the y-axis.
#' @param title         a character string specifying the text for the title for
#'                      the plot.
#' @param subtitle      a character string specifying the text for the subtitle
#'                      for the plot.
#' @param filename      a character string indicating the \code{filename}
#'                      argument including the file extension in the \code{ggsave}
#'                      function. Note that one of \code{".eps"}, \code{".ps"},
#'                      \code{".tex"}, \code{".pdf"} (default),
#'                      \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                      \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                      to be specified as file extension in the \code{file}
#'                      argument. Note that plots can only be saved when
#'                      \code{plot = TRUE}.
#' @param width         a numeric value indicating the \code{width} argument
#'                      (default is the size of the current graphics device)
#'                      for the \code{ggsave} function.
#' @param height        a numeric value indicating the \code{height} argument
#'                      (default is the size of the current graphics device)
#'                      for the \code{ggsave} function.
#' @param units         a character string indicating the \code{units} argument
#'                      (default is \code{in}) for the \code{ggsave} function.
#' @param dpi           a numeric value indicating the \code{dpi} argument
#'                      (default is \code{600}) for the \code{ggsave} function.
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
#' @details
#' Levene's test is equivalent to a one-way analysis of variance (ANOVA) with the
#' absolute deviations of observations from the mean of each group as dependent
#' variable (\code{center = "mean"}). Brown and Forsythe (1974) modified the
#' Levene's test by using the absolute deviations of observations from the median
#' (\code{center = "median"}). By default, the Levene's test uses the absolute
#' deviations of observations from the median.
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
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame with the outcome and grouping variable}
#' \item{\code{formula}}{formula}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{result}}{result table}
#'
#' @export
#'
#' @examples
#' # Example 1: Levene's test based on the median
#' test.levene(mpg ~ gear, data = mtcars)
#'
#' # Example 2: Levene's test based on the arithmetic mean
#' test.levene(mpg ~ gear, data = mtcars, method = "mean")
#'
#' # Example 3: Levene's test based on the median, plot results
#' test.levene(mpg ~ gear, data = mtcars, plot = TRUE)
#'
#' # Example 4: Write results into a text file
#' test.levene(mpg ~ gear, data = mtcars, write = "Levene.txt")

#' # Example 5: Levene's test based on the median, save plot
#' test.levene(mpg ~ gear, data = mtcars, plot = TRUE,
#'             filename = "Levene-test.png", dpi = 600, width = 6, height = 5)
test.levene <- function(formula, data, method = c("median", "mean"),
                        conf.level = 0.95, hypo = TRUE, descript = TRUE,
                        digits = 2, p.digits = 3, as.na = NULL, plot = FALSE,
                        violin = TRUE, box = TRUE, jitter = FALSE, violin.alpha = 0.3,
                        violin.trim = FALSE, box.alpha = 0.2, box.width = 0.2,
                        jitter.size = 1.25, jitter.width = 0.05, jitter.height = 0,
                        jitter.alpha = 0.2, gray = FALSE, start = 0.9, end = 0.4,
                        color = NULL, xlab = NULL, ylab = NULL, ylim = NULL,
                        ybreaks = ggplot2::waiver(), title = "", subtitle = "",
                        filename = NULL, width = NA, height = NA, units = c("in", "cm", "mm", "px"),
                        dpi = 600, write = NULL, append = TRUE, check = TRUE, output = TRUE) {

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

  data <- as.data.frame(data)

  # Check if variables are in the data
  (!var.formula %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(y)], collapse = ", ")), call. = FALSE) })()

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

  # Check inputs
  .check.input(logical = c("hypo", "descript", "plot", "violin", "box", "jitter", "violin.trim", "gray", "append", "output"),
               numeric = list(violin.alpha = 1L, box.alpha = 1L, box.width = 1L, jitter.size = 1L, jitter.width = 1L, jitter.height = 1L, jitter.alpha = 1L, start = 1L, end = 1L, ylim = 2L),
               character = list(xlab = 1L, ylab = 1L, title = 1L, subtitle = 1L),
               s.character = list(method = c("median", "mean")),
               args = c("conf.level", "digits", "p.digits", "write1"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Package ggplot2
    if (isTRUE(plot)) { if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed to draw a plot, please install the package.", call. = FALSE) } }

    # Variance zero
    (tapply(data[, y.var], data[, group.var], var, na.rm = TRUE) == 0L) |> (\(y) if (isTRUE(any(y))) { stop(paste0("There are groups with 0 variance: Group ", paste(which(y), collapse = ", ")), call. = FALSE) })()

    # Check input 'start'
    if (isTRUE(start < 0L || start > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'start'", call. = FALSE) }

    # Check input 'end'
    if (isTRUE(end < 0L || end > 1L)) { stop("Please specify a numeric value between 0 and 1 for the argument 'end'", call. = FALSE) }

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

  result.ci <- misty::ci.var(y, group = group, conf.level = conf.level, output = FALSE)$result |>
    (\(y) data.frame(y[, c("group", "n", "nNA", "m")], sd = sqrt(y$var), y[, c("var", "low", "upp", "skew", "kurt")]))()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Analysis of Variance ####

  # Median by grouping variable
  switch(method, "median" ={

    y.center <- tapply(y, group, median)

  # Mean by grouping variable
  }, "mean" = {

    y.center <- tapply(y, group, mean)

  })

  # Deviation from the median or mean
  y.dev <- abs(y - y.center[match(group, names(y.center))])

  # Analysis of Variance
  result.aov <- summary(aov(y.dev ~ as.factor(group)))[[1L]]

  row.names(result.aov) <- c("Group", "Residuals")

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "test.levene",
                 data = data.frame(y, group = factor(group)),
                 formula = formula,
                 args = list(method = method, conf.level = conf.level, hypo = hypo,
                             descript = descript, digits = digits, p.digits = p.digits,
                             as.na = as.na, plot = plot, violin = violin, box = box,
                             jitter = jitter, violin.alpha = violin.alpha, violin.trim = violin.trim,
                             box.alpha = box.alpha, box.width = box.width, jitter.size = jitter.size,
                             jitter.width = jitter.width, jitter.height = jitter.height,
                             jitter.alpha = jitter.alpha, gray = gray, start = start, end = end,
                             color = color, xlab = xlab, ylab = ylab, ylim = ylim, ybreaks = ybreaks,
                             title = title, subtitle = subtitle, filename = filename,
                             width = width, height = height, units = units, dpi = dpi,
                             write = write, append = append, check = check, output = output),
                 plot = NULL, result = list(descript = result.ci, test = result.aov))

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
