#' Levene's Test for Homogeneity of Variance
#'
#' This function performs Levene's test for homogeneity of variance across two
#' or more independent groups including a plot showing violin plots and boxplots
#' representing the distribution of the outcome variable for each group.
#'
#' @param formula     a formula of the form \code{y ~ group} where \code{y} is
#'                    a numeric variable giving the data values and \code{group}
#'                    a numeric variable, character variable or factor with two
#'                    or more than two values or factor levels giving the
#'                    corresponding groups.
#' @param data        a matrix or data frame containing the variables in the
#'                    formula \code{formula}.
#' @param method      a character string specifying the method to compute the
#'                    center of each group, i.e. \code{method = "median"} (default)
#'                    to compute the Levene's test based on the median (aka
#'                    Brown-Forsythe test) or \code{method = "mean"} to compute
#'                    the Levene's test based on the arithmetic mean.
#' @param hypo        logical: if \code{TRUE}, null and alternative hypothesis
#'                    are shown on the console.
#' @param descript    logical: if \code{TRUE}, descriptive statistics are shown
#'                    on the console.
#' @param conf.level  a numeric value between 0 and 1 indicating the confidence
#'                    level of the interval.
#' @param digits      an integer value indicating the number of decimal places
#'                    to be used for displaying results.
#' @param p.digits    an integer value indicating the number of decimal places
#'                    to be used for displaying the \emph{p}-value.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before
#'                    conducting the analysis.
#' @param plot        logical: if \code{TRUE}, a plot showing violins with
#'                    boxplots is drawn.
#' @param violin      logical: if \code{TRUE} (default), violins are drawn.
#' @param box         logical: if \code{TRUE} (default), boxplots are drawn.
#' @param jitter      logical: if \code{TRUE} (default), jittered data points
#'                    are drawn.
#' @param gray        logical: if \code{TRUE}, the plot is drawn in gray scale.
#' @param filename    a character string indicating the \code{filename}
#'                    argument including the file extension in the \code{ggsave}
#'                    function. Note that one of \code{".eps"}, \code{".ps"},
#'                    \code{".tex"}, \code{".pdf"} (default), \code{".jpeg"},
#'                    \code{".tiff"}, \code{".png"}, \code{".bmp"}, \code{".svg"}
#'                    or \code{".wmf"} needs to be specified as file extension
#'                    in the \code{file} argument. Note that plots can only be
#'                    saved when specifying \code{plot = TRUE}.
#' @param width       a numeric value indicating the \code{width} argument (default
#'                    is the size of the current graphics device) for the \code{ggsave}
#'                    function.
#' @param height      a numeric value indicating the \code{height} argument
#'                    (default is the size of the current graphics device)
#'                    for the \code{ggsave} function.
#' @param dpi         a numeric value indicating the \code{dpi} argument
#'                    (default: \code{600}) for the \code{ggsave} function.
#' @param write       a character string naming a file for writing the output into
#'                    either a text file with file extension \code{".txt"} (e.g.,
#'                    \code{"Output.txt"}) or Excel file with file extension
#'                    \code{".xlsx"} (e.g., \code{"Output.xlsx"}). If the file name
#'                    does not contain any file extension, an Excel file will be
#'                    written.
#' @param append      logical: if \code{TRUE} (default), output will be appended
#'                    to an existing text file with extension \code{.txt} specified
#'                    in \code{write}, if \code{FALSE} existing text file will
#'                    be overwritten.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#' @param output      logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{aov.b}}, \code{\link{test.t}}, \code{\link{test.welch}}
#'
#' @references
#' Brown, M. B., & Forsythe, A. B. (1974). Robust tests for the equality of variances.
#' \emph{Journal of the American  Statistical Association, 69}, 364-367.
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
#' #————————————————————————————————————————————————————————————————————————————
#' # Levene's Test
#'
#' # Example 1a: Levene's test based on the median
#' test.levene(mpg ~ gear, data = mtcars)
#'
#' # Example 1b: Levene's test based on the arithmetic mean
#' test.levene(mpg ~ gear, data = mtcars, method = "mean")
#'
#' # Example 1c: Levene's test, print descriptive statistics
#' test.levene(mpg ~ gear, data = mtcars, descript = TRUE)
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Plot
#'
#' # Example 2a: Plot results, default setting
#' test.levene(mpg ~ gear, data = mtcars, plot = TRUE)
#'
#' # Example 2b: Plot results, no violin plots, draw jittered data points
#' test.levene(mpg ~ gear, data = mtcars, plot = TRUE, violin = FALSE, jitter = TRUE)
#'
#' # Example 2c: Plot results using the plot() function, use additional arguments
#' # see Details in the help page of the function plot.misty.object
#' object <- test.levene(mpg ~ gear, data = mtcars)
#' plot(object, violin.alpha = 0.1, box.width = 0.1, title = "Levene's Test")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Create Plot Manually
#'
#' # Load ggplot2 package
#' library(ggplot2)
#'
#' # Create misty object
#' object <- test.levene(mpg ~ gear, data = mtcars)
#'
#' # Example 3: Plot
#' ggplot(object$data, aes(group, y, fill = group)) +
#'   geom_violin(alpha = 0.3, trim = FALSE) +
#'   geom_boxplot(alpha = 0.2, width = 0.2) +
#'   geom_jitter(alpha = 0.2, width = 0.05, height = 0, size = 1.25) +
#'   theme_bw() +
#'    ggplot2::guides(fill = "none")
#'
#' #————————————————————————————————————————————————————————————————————————————
#' # Write Results and Save Plot
#'
#' \dontrun{
#'
#' # Example 4a: Write results into a text file
#' test.levene(mpg ~ gear, data = mtcars, write = "Levene.txt")
#'
#' # Example 4b: Write results into an Excel file
#' test.levene(mpg ~ gear, data = mtcars, write = "Levene.xlsx")
#'
#' # Example 4c: Save plot as PNG fine
#' test.levene(mpg ~ gear, data = mtcars, plot = TRUE,
#'             filename = "Levene-Test.png", width = 6, height = 5)
#' }
test.levene <- function(formula, data, method = c("median", "mean"),
                        hypo = FALSE, descript = FALSE, conf.level = 0.95,
                        digits = 2, p.digits = 3, as.na = NULL, plot = FALSE,
                        violin = TRUE, box = TRUE, jitter = FALSE, gray = FALSE,
                        filename = NULL, width = NA, height = NA, dpi = 600,
                        write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'formula' is missing
  if (isTRUE(missing(formula))) { stop("Please specify a formula using the argument 'formula'", call. = FALSE) }

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a matrix or data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Formula and Data Frame -----------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variables ####

  var.formula <- all.vars(as.formula(formula))

  # Grouping variable
  group.var <- attr(terms(formula[-2L]), "term.labels")

  # Outcome
  y.var <- setdiff(var.formula, group.var)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data Frame ####

  # As data frame
  data <- as.data.frame(data)

  # Check if variables are in the data
  (!var.formula %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in the the formula were not found in 'data': ", paste(var.formula[which(y)], collapse = ", ")), call. = FALSE) })()

  # Check if input 'formula' has only one outcome variable
  if (isTRUE(length(y.var) != 1L)) { stop("Please specify a formula with only one outcome variable.", call. = FALSE) }

  # Check if input 'formula' has only one grouping variable
  if (isTRUE(length(group.var) != 1L)) { stop("Please specify a formula with only one grouping variable.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert User-Missing Values into NA ####

  if (isTRUE(!is.null(as.na))) { data[, y.var] <- .as.na(data[, y.var], na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise Deletion ####

  data <- na.omit(data[, var.formula])

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("hypo", "descript", "plot", "violin", "box", "jitter", "append", "output"), s.character = list(method = c("median", "mean")),
               args = c("conf.level", "digits", "p.digits", "write2"), envir = environment(), input.check = check)

  # Check if variance is zero
  if (isTRUE(check)) { (tapply(data[, y.var], data[, group.var], var, na.rm = TRUE) == 0L) |> (\(y) if (isTRUE(any(y))) { stop(paste0("There are groups with 0 variance: Group ", paste(which(y), collapse = ", ")), call. = FALSE) })() }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # 'method' Argument
  if (isTRUE(all(c("median", "mean") %in% method))) { method <- "median" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Outcome
  y <- data[, y.var]

  # Grouping variable
  group <- data[, group.var]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive Statistics ####

  result.descript <- misty::ci.var(y, group = group, conf.level = conf.level, output = FALSE)$result |> (\(y) data.frame(y[, c("group", "n", "nNA", "m")], sd = sqrt(y$var), y[, c("var", "low", "upp", "skew", "kurt")]))()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Levene's Test ####

  # Median by grouping variable
  switch(method, "median" ={

    y.center <- tapply(y, group, median, na.rm = TRUE)

  # Mean by grouping variable
  }, "mean" = {

    y.center <- tapply(y, group, mean, na.rm = TRUE)

  })

  # Deviation from the median or mean
  y.dev <- abs(y - y.center[match(group, names(y.center))])

  # Analysis of Variance
  result.aov <- setNames(summary(aov(y.dev ~ as.factor(group)))[[1L]], nm = c("df", "SS", "MSS", "F", "p")) |> (\(p) rbind(p, colSums(p)))()

  row.names(result.aov) <- c("Group", "Residuals", "Total")

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "test.levene",
                 data = data.frame(y, group = factor(group)),
                 formula = formula,
                 args = list(method = method, conf.level = conf.level, hypo = hypo, descript = descript, digits = digits, p.digits = p.digits, as.na = as.na, plot = plot, violin = violin, box = box, jitter = jitter, filename = filename, width = width, height = height, dpi = dpi, write = write, append = append, check = check, output = output),
                 plot = NULL,
                 result = list(descript = result.descript, test = result.aov))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Plot ---------------------------------------------------------

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  # Write Results ——————————————————————————————————————————————————————————————

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
