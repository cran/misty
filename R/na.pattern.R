#' Missing Data Pattern
#'
#' This function computes a summary of missing data patterns, i.e., number (%)
#' of cases with a specific missing data pattern and plots the missing data
#' patterns.
#'
#' @param ...               a matrix or data frame with incomplete data, where
#'                          missing values are coded as \code{NA}. Alternatively,
#'                          an expression indicating the variable names in
#'                          \code{data} e.g., \code{na.pattern(x1, x2, x3, data = dat)}.
#'                          Note that the operators \code{.}, \code{+}, \code{-},
#'                          \code{~}, \code{:}, \code{::}, and \code{!} can also
#'                          be used to select variables, see 'Details' in the
#'                          \code{\link{df.subset}} function.
#' @param data              a data frame when specifying one or more variables
#'                          in the argument \code{...}. Note that the argument
#'                          is \code{NULL} when specifying a matrix or data frame
#'                          for the argument \code{...}.
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
#' @param fill.col          a character string indicating the color for the
#'                          \code{"fill"} argument. Note that the first color
#'                          represents missing values and the second color
#'                          represent observed values.
#' @param alpha             a numeric value between 0 and 1 for the \code{alpha}
#'                          argument (default is \code{0.1}.
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
#' @param saveplot          logical: if \code{TRUE}, the ggplot is saved.
#' @param file              a character string indicating the \code{filename}
#'                          argument (default is \code{"NA_Pattern.pdf"}) including
#'                          the file extension for the \code{ggsave} function.
#'                          Note that one of \code{".eps"}, \code{".ps"},
#'                          \code{".tex"}, \code{".pdf"} (default), \code{".jpeg"},
#'                          \code{".tiff"}, \code{".png"}, \code{".bmp"},
#'                          \code{".svg"} or \code{".wmf"} needs to be specified
#'                          as file extension in the \code{file} argument.
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
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
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
#' \item{\code{data}}{list with data frames, i.e., \code{data} for the data frame
#'                    with variables used in the current analysis, and \code{plotdat}
#'                    for the data frame used for plotting the results}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#' \item{\code{plot}}{ggplot2 object for plotting the results}
#' \item{\code{pattern}}{a numeric vector indicating the missing data pattern for each case}
#'
#' @export
#'
#' @examples
#' # Example 1a: Compute a summary of missing data patterns
#' dat.pattern <- na.pattern(airquality)
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' dat.pattern <- na.pattern(., data = airquality)
#'
#' # Example 2a: Compute and plot a summary of missing data patterns
#' na.pattern(airquality, plot = TRUE)
#'
#' # Example 2b: Exclude missing data patterns with less than 3 cases
#' na.pattern(airquality, plot = TRUE, n.pattern = 3)
#'
#' # Example 3: Vector of missing data pattern for each case
#' dat.pattern$pattern
#
#' # Data frame without cases with missing data pattern 2 and 4
#' airquality[!dat.pattern$pattern %in% c(2, 4), ]
#'
#' \dontrun{
#' # Example 4a: Write Results into a text file
#' result <- na.pattern(airquality, write = "NA_Pattern.xlsx")
#'
#' # Example 4b: Write Results into a Excel file
#' result <- na.pattern(airquality, write = "NA_Pattern.xlsx")
#'
#' result <- 4c.pattern(dat, output = FALSE)
#' write.result(result, "NA_Pattern.xlsx")
#' }
na.pattern <- function(..., data = NULL, order = FALSE, n.pattern = NULL, plot = FALSE,
                       square = TRUE, rotate = FALSE, fill.col = c("#B61A51B3", "#006CC2B3"),
                       alpha = 0.6, plot.margin = c(4, 16, 0, 4), legend.box.margin = c(-8, 6, 6, 6),
                       legend.key.size = 12, legend.text.size = 9, saveplot = FALSE, file = "NA_Patternt.pdf",
                       width = NA, height = NA, units = c("in", "cm", "mm", "px"), dpi = 600,
                       digits = 2, as.na = NULL, write = NULL, append = TRUE, check = TRUE,
                       output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a matrix or data frame")

    # Extract data
    x <- data[, var.names]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'order'
    if (isTRUE(!is.logical(order))) { stop("Please specify TRUE or FALSE for the argument 'order'.", call. = FALSE) }

    # Check input 'n.pattern'
    if (isTRUE(n.pattern %% 1L != 0L || n.pattern < 0L)) { stop("Please specify a positive integer value for the argument 'n.pattern'.", call. = FALSE) }

    # Check input 'plot'
    if (isTRUE(!is.logical(plot))) { stop("Please specify TRUE or FALSE for the argument 'plot'.", call. = FALSE) }

    if (isTRUE(plot)) {

      # Check input 'rotate'
      if (isTRUE(!is.logical(rotate))) { stop("Please specify TRUE or FALSE for the argument 'rotate'.", call. = FALSE) }

      # Check input 'square'
      if (isTRUE(!is.logical(square))) { stop("Please specify TRUE or FALSE for the argument 'square'.", call. = FALSE) }

      # Check input 'fill.col'
      if (isTRUE(length(fill.col) != 2L)) { stop("Please specify character vector with two elements for the argument 'fill.col'.", call. = FALSE) }

      # Check input 'alpha'
      if (isTRUE(alpha <= 0L || alpha > 1L)) { stop("Please specify a value between 0 aund 1 for the argument 'alpha'.", call. = FALSE) }

      # Check input 'plot.margin'
      if (isTRUE(length(plot.margin) != 4L)) { stop("Please specify a numeric vector with four elements for the argument 'plot.margin'.", call. = FALSE) }

      # Check input 'legend.box.margin'
      if (isTRUE(length(legend.box.margin) != 4L)) { stop("Please specify a numeric vector with four elements for the argument 'legend.box.margin'.", call. = FALSE) }

    }

    # Check input 'saveplot'
    if (isTRUE(!is.logical(saveplot))) { stop("Please specify TRUE or FALSE for the argument 'saveplot'.", call. = FALSE) }

    if (isTRUE(saveplot)) {

      # Check input 'file'
      if (isTRUE(!grepl(".", file) || !rev(unlist(strsplit(file, "\\.")))[1L] %in% c("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf"))) { stop("Please specify a character string including the file extension (e.g., \".pdf\" or \".png\") for the argument 'file'.", call. = FALSE) }

      # Check input 'units'
      if (isTRUE(!all(units %in% c("in", "cm", "mm", "px")))) { stop("Character string in the argument 'units' does not match with \"in\", \"cm\", \"mm\", \"pdf\", or \"px\".", call. = FALSE) }

    }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Please specify a positive integer value for the argument 'digits'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

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
    pattern <- apply(x.na.order.dupl, 1L, function(y) paste(as.numeric(y), collapse = "")) |>
      (\(z) unname(vapply(patt, function(y) match(y, table = z), FUN.VALUE = 1L)))()

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
  # Plot -----------------------------------------------------------------------

  if (isTRUE(plot)) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot Data ####

    obs_miss <- NULL

    plotdat <- do.call("rbind", apply(restab[-nrow(restab), c("pattern", colnames(x.na))], 1, function(y) {

      data.frame(pattern = y["pattern"],
                 var = colnames(x.na),
                 obs_miss = y[colnames(x.na)],
                 x = seq_len(ncol(x.na)), row.names = NULL)

    }))

    # Factor
    plotdat$obs_miss <- factor(ifelse(plotdat$obs_miss == 1L, "Observed", "Missing"))

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Create ggplot ####

    p <- ggplot2::ggplot(plotdat, ggplot2::aes(x, pattern, fill = obs_miss, alpha = alpha)) +
      ggplot2::geom_tile(color = "black") +
      ggplot2::scale_fill_manual(values = c("Missing" = fill.col[1L], "Observed" = fill.col[2L])) +
      ggplot2::scale_alpha_continuous(limits = c(0, 1), guide = "none") +
      ggplot2::scale_x_continuous("Number of Missing Entries per Variable",
                                  breaks = seq_len(length(colnames(x.na))), labels = restab[nrow(restab), colnames(x.na)],
                                  sec.axis = ggplot2::dup_axis(labels = colnames(x.na), name = "Variable")) +
      ggplot2::scale_y_reverse("Number of Missing Entries per Pattern",
                               breaks = seq_len(length(restab$n) - 1L), labels = restab[-nrow(restab), "n"],
                               sec.axis = ggplot2::dup_axis(labels = restab[-nrow(restab), "nNA"])) +
      ggplot2::ylab("Pattern Frequency") +
      ggplot2::theme(plot.margin = ggplot2::margin(t = plot.margin[1L], r = plot.margin[2L], b = plot.margin[3L], l = plot.margin[4L]),
                     legend.title = ggplot2::element_blank(),
                     legend.position = "bottom",
                     legend.key.size = ggplot2::unit(legend.key.size, "pt"),
                     legend.text = ggplot2::element_text(size = legend.text.size),
                     legend.box.margin = ggplot2::margin(t = legend.box.margin[1L], r = legend.box.margin[2L], b = legend.box.margin[3L], l = legend.box.margin[4L]),
                     panel.grid.minor = ggplot2::element_blank(), panel.grid.major = ggplot2::element_blank())

    ### Square Plot Tiles ####
    if (isTRUE(square)) {

      p <- p + ggplot2::coord_fixed(expand = FALSE)

    } else {

      p <- p + ggplot2::coord_cartesian(expand = FALSE)

    }

    ### Rotate Labels ####
    if (isTRUE(rotate)) { p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90L)) }

    ### Caption ####
    if (isTRUE(length(n.pattern.exclude) != 0L)) {

      p <- p + ggplot2::labs(caption = paste0("Note. ", length(n.pattern.exclude), ifelse(length(n.pattern.exclude) == 1L, " pattern ", " patterns "), " with less than ", n.pattern, " cases removed.")) +
             ggplot2::theme(plot.caption = ggplot2::element_text(hjust = 0.5, vjust = 5))

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Plot ggplot Object ####

    print(suppressWarnings(suppressMessages(p)))

  } else {

    p <- plotdat <- NULL

  }

  #_____________________________________________________________________________
  #
  # Save Plot ------------------------------------------------------------------

  if (isTRUE(saveplot)) { suppressWarnings(suppressMessages(ggplot2::ggsave(file, plot = p, width = width, height = height, units = units, dpi = dpi))) }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "na.pattern",
                 data = list(data = x, plotdat = plotdat),
                 args = list(order = order, n.pattern = NULL, plot = plot, square = square,
                             rotate = rotate, fill.col = fill.col, alpha = alpha,
                             plot.margin = plot.margin, legend.box.margin = legend.box.margin,
                             legend.key.size = legend.key.size, legend.text.size = legend.text.size,
                             saveplot = saveplot, file = file, width = width, height = height, units = units,
                             dpi = dpi, digits = digits, as.na = as.na, write = write, append = append,
                             check = check, output = output),
                 result = restab, plot = p, pattern = pattern)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Write results ####

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    if (isTRUE(grepl("\\.txt", write))) {

      # Send R output to textfile
      sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

      if (isTRUE(append && file.exists(write))) { write("", file = write, append = TRUE) }

      # Print object
      print(object, check = FALSE)

      # Close file connection
      sink()

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Excel file ####

    } else {

      misty::write.result(object, file = write)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Output ####

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
