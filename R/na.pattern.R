#' Missing Data Pattern
#'
#' This function computes a summary of missing data patterns, i.e., number (%)
#' of cases with a specific missing data pattern and plots the missing data
#' patterns.
#'
#' @param data              a data frame with incomplete data, where missing
#'                          values are coded as \code{NA}.
#' @param ...               an expression indicating the variable names in
#'                          \code{data} e.g., \code{na.pattern(dat, x1, x2, x3)}.
#'                          Note that the operators \code{+}, \code{-},
#'                          \code{~}, \code{:}, \code{::}, and \code{!} can also
#'                          be used to select variables, see 'Details' in the
#'                          \code{\link{df.subset}} function.
#' @param order             logical: if \code{TRUE}, variables are ordered from
#'                          left to right in increasing order of missing values.
#' @param n.pattern         an integer value indicating the minimum number of
#'                          cases sharing a missing data pattern to be included
#'                          in the result table and the plot, e.g., specifying
#'                          \code{n.pattern = 5} excludes missing data patters
#'                          with less than \code{5} cases.
#' @param digits            an integer value indicating the number of decimal
#'                          places to be used for displaying percentages.
#' @param as.na             a numeric vector indicating user-defined missing
#'                          values, i.e. these values are converted to NA before
#'                          conducting the analysis.
#' @param plot              logical: if \code{TRUE}, missing data pattern is
#'                          plotted.
#' @param square            logical: if \code{TRUE} (default), the plot tiles
#'                          are squares to mimic the \code{md.pattern} function
#'                          in the package \pkg{mice}.
#' @param rotate            logical: if \code{TRUE}, the variable name labels
#'                          are rotated 90 degrees.
#' @param color             a character string indicating the color for the
#'                          \code{"fill"} argument. Note that the first color
#'                          represents missing values and the second color
#'                          represent observed values.
#' @param tile.alpha        a numeric value between 0 and 1 for the \code{alpha}
#'                          argument (default is \code{0.1}).
#' @param plot.margin       a numeric vector indicating the \code{plot.margin}
#'                          argument for the \code{theme} function.
#' @param legend.box.margin a numeric vector indicating the \code{legend.box.margin}
#'                          argument for the \code{theme} function.
#' @param legend.key.size   a numeric value indicating the \code{legend.key}
#'                          argument (default is \code{unit(12, "pt")})
#'                          for the \code{theme} function.
#' @param legend.text.size  a numeric value indicating the \code{legend.text}
#'                          argument (default is \code{element_text(size = 10)})
#'                          for the \code{theme} function.
#' @param filename          a character string indicating the \code{filename}
#'                          argument including the file extension in the \code{ggsave}
#'                          function. Note that one of \code{".eps"}, \code{".ps"},
#'                          \code{".tex"}, \code{".pdf"} (default),
#'                          \code{".jpeg"}, \code{".tiff"}, \code{".png"},
#'                          \code{".bmp"}, \code{".svg"} or \code{".wmf"} needs
#'                          to be specified as file extension in the \code{file}
#'                          argument. Note that plots can only be saved when
#'                          \code{plot = TRUE}.
#' @param width             a numeric value indicating the \code{width} argument
#'                          (default is the size of the current graphics device)
#'                          for the \code{ggsave} function.
#' @param height            a numeric value indicating the \code{height} argument
#'                          (default is the size of the current graphics device)
#'                          for the \code{ggsave} function.
#' @param units             a character string indicating the \code{units} argument
#'                          (default is \code{in}) for the \code{ggsave} function.
#' @param dpi               a numeric value indicating the \code{dpi} argument
#'                          (default is \code{600}) for the \code{ggsave} function.
#' @param write             a character string naming a file for writing the output
#'                          into either a text file with file extension \code{".txt"}
#'                          (e.g., \code{"Output.txt"}) or Excel file with file
#'                          extension \code{".xlsx"}  (e.g., \code{"Output.xlsx"}).
#'                          If the file name does not contain any file extension,
#'                          an Excel file will be written.
#' @param append            logical: if \code{TRUE} (default), output will be
#'                          appended to an existing text file with extension
#'                          \code{.txt} specified in \code{write}, if \code{FALSE}
#'                          existing text file will be overwritten.
#' @param check             logical: if \code{TRUE} (default), argument
#'                          specification is checked.
#' @param output            logical: if \code{TRUE} (default), output is shown.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.auxiliary}},
#' \code{\link{na.coverage}}, \code{\link{na.descript}}, \code{\link{na.indicator}},
#' \code{\link{na.prop}}, \code{\link{na.test}}, \code{\link{write.result}}
#'
#' @references
#' Enders, C. K. (2022). \emph{Applied missing data analysis} (2nd ed.). The Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576.
#' https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' Oberman, H. (2023). \emph{ggmice: Visualizations for 'mice' with 'ggplot2'}.
#' R package version 0.1.0.	https://doi.org/10.32614/CRAN.package.ggmice
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.).
#' Chapman & Hall.
#'
#' @note The code for plotting missing data patterns is based on the \code{plot_pattern}
#' function in the \pkg{ggmice} package by Hanne Oberman.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame with variables used in the analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{pattern}}{a numeric vector indicating the missing data pattern for each case}
#'
#' @export
#'
#' @examples
#' # Example 1: Compute a summary of missing data patterns
#' dat.pattern <- na.pattern(airquality)
#'
#' # Example 2a: Compute and plot a summary of missing data patterns
#' na.pattern(airquality, plot = TRUE)
#'
#' # Example 2b: Exclude missing data patterns with less than 3 cases
#' na.pattern(airquality, plot = TRUE, n.pattern = 3)
#'
#' # Example 3: Vector of missing data pattern for each case
#' dat.pattern$pattern
#'
#' # Data frame without cases with missing data pattern 2 and 4
#' airquality[!dat.pattern$pattern == 2, ]
#'
#' # Example 4a: Write Results into a text file
#' na.pattern(airquality, write = "NA_Pattern.xlsx")
#'
#' # Example 4b: Write Results into a Excel file
#' na.pattern(airquality, write = "NA_Pattern.xlsx")
na.pattern <- function(data, ..., order = FALSE, n.pattern = NULL, digits = 2,
                       as.na = NULL,plot = FALSE, square = TRUE, rotate = FALSE,
                       color = c("#B61A51B3", "#006CC2B3"), tile.alpha = 0.6,
                       plot.margin = c(4, 16, 0, 4), legend.box.margin = c(-8, 6, 6, 6),
                       legend.key.size = 12, legend.text.size = 9, filename = NULL,
                       width = NA, height = NA, units = c("in", "cm", "mm", "px"),
                       dpi = 600, write = NULL, append = TRUE, check = TRUE,
                       output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- as.data.frame(data[, .var.names(data = data, ...), drop = FALSE])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("order", "plot", "rotate", "square", "append", "output"),
               numeric = list(n.pattern = 1L, plot.margin = 4L, legend.box.margin = 4L, legend.key.size = 1L, legend.text.size = 1L, dpi = 1L),
               character = list(filename = 1L), args = c("tile.alpha", "units", "digits", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Package ggplot2
    if (isTRUE(plot)) { if (isTRUE(!nzchar(system.file(package = "ggplot2")))) { stop("Package \"ggplot2\" is needed to draw a plot, please install the package.", call. = FALSE) } }

    # Check input 'n.pattern'
    if (isTRUE(n.pattern %% 1L != 0L || n.pattern < 0L)) { stop("Please specify a positive integer value for the argument 'n.pattern'.", call. = FALSE) }

    # Check input 'color'
    if (isTRUE(length(color) != 2L)) { stop("Please specify character vector with two elements for the argument 'color'.", call. = FALSE) }

    # Check input 'filename'
    if (isTRUE(!is.null(filename))) { if (isTRUE(!grepl(".", filename) || !rev(unlist(strsplit(filename, "\\.")))[1L] %in% c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"))) { stop("Please specify a character string including the file extension (e.g., \".pdf\" or \".png\") for the argument 'filename'.", call. = FALSE) } }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'units' Argument ####

  # Default setting
  if (isTRUE(all(c("in", "cm", "mm", "px") %in% units))) { units <- "in" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Missing data TRUE/FALSE matrix
  x.na <- is.na(x)

  # Order variables from left to right in increasing order of missing values
  if (isTRUE(order)) { x.na <- x.na[, order(colSums(x.na))] }

  # Missing data pattern
  patt <- apply(x.na, 1L, function(y) paste(as.numeric(y), collapse = ""))

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Patterns ####

  if (isTRUE(!is.null(n.pattern))) {

    # Patterns excluded
    n.pattern.exclude <- names(which(table(patt) < n.pattern))

    # Indices of patterns excluded
    pattern.exclude <- which(patt %in% n.pattern.exclude)

    # Exclude patterns
    if (isTRUE(length(n.pattern.exclude) > 0L)) {

      patt <- patt[-pattern.exclude]
      x.na <- x.na[-pattern.exclude, ]

      if (isTRUE(length(patt) == 0L)) { stop(paste0("No missing data pattern lefts after removing patterns with less than ", n.pattern, " cases."), call. = FALSE) }

      if (isTRUE(length(n.pattern.exclude) != 0L)) { warning(paste0(length(n.pattern.exclude), " missing data", ifelse(length(n.pattern.exclude) == 1L, " pattern ", " patterns "), "with less than ", n.pattern, " cases", ifelse(length(n.pattern.exclude) == 1L, " was ", " were "), "removed from the analysis."), call. = FALSE) }

    }

  } else {

    n.pattern.exclude <- NULL

  }

  # Order NA matrix
  x.na.order <- x.na[order(patt), ]

  # Remove duplicated rows
  x.na.order.dupl <- x.na.order[!duplicated(x.na.order), ]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Result Table ####

  if (isTRUE(!is.null(dim(x.na.order.dupl)))) {

    restab <- rbind(data.frame(pattern = seq_len(nrow(x.na.order.dupl)),
                               n = as.vector(table(patt)),
                               perc = as.vector(table(patt) / nrow(x.na) * 100L),
                               abs(x.na.order.dupl - 1L),
                               nNA = rowSums(x.na.order.dupl),
                               pNA = rowSums(x.na.order.dupl) / ncol(x.na) * 100L,
                               row.names = NULL, stringsAsFactors = FALSE),
                    c(NA, sum(as.vector(table(patt))), sum(as.vector(table(patt) / nrow(x.na) * 100L)), colSums(x.na), NA, NA))

    # Number of missing data pattern
    pattern <- apply(x.na.order.dupl, 1L, function(y) paste(as.numeric(y), collapse = "")) |> (\(z) unname(vapply(patt, function(y) match(y, table = z), FUN.VALUE = 1L)))()

  } else {

    restab <- rbind(data.frame(pattern = 1L,
                               n = as.vector(table(patt)),
                               perc = as.vector(table(patt) / nrow(x.na) * 100L),
                               matrix(abs(x.na.order.dupl - 1L), ncol = length(x.na.order.dupl), dimnames = list(NULL, names(x.na.order.dupl))),
                               nNA = sum(x.na.order.dupl),
                               pNA = sum(x.na.order.dupl) / ncol(x.na) * 100L,
                               row.names = NULL, stringsAsFactors = FALSE),
                    c(NA, sum(as.vector(table(patt))), sum(as.vector(table(patt) / nrow(x.na) * 100L)), colSums(x.na), NA, NA))

    pattern <- rep(1L, times = nrow(x))

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "na.pattern",
                 data = x,
                 args = list(order = order, n.pattern = NULL, digits = digits, as.na = as.na,
                             plot = plot, square = square, rotate = rotate, color = color,
                             tile.alpha = tile.alpha, plot.margin = plot.margin, legend.box.margin = legend.box.margin,
                             legend.key.size = legend.key.size, legend.text.size = legend.text.size,
                             filename = filename, width = width, height = height, units = units, dpi = dpi,
                             write = write, append = append, check = check, output = output),
                 plot = NULL, result = restab, pattern = pattern)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Plot and Save Results ------------------------------------------------------

  if (isTRUE(plot)) { object$plot <- plot(object, filename = filename, width = width, height = height, units = units, dpi = dpi, check = FALSE) |> (\(y) suppressMessages(suppressWarnings(print(y))))() }

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output ####

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
