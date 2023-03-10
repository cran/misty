#' Open R Script, R Markdown script, or SQL Script in RStudio
#'
#' This function is used to open an R script, R markdown script, or SQL script
#' in RStudio.
#'
#' The function \code{documentOpen()} in the package \pkg{rstudioapi} is used to
#' open an R script.
#
#' @param path       a character string indicating the path of the script.
#' @param line       a numeric value indicating the line in the script to navigate
#'                   to.
#' @param col        a numeric value indicating the column in the script to
#'                   navigate to.
#' @param cursor     logical: if \code{TRUE} (default), the cursor moves to the
#'                   requested location after opening the document.
#' @param run        logical: if \code{TRUE}, the code is executed after the
#'                   document is opened
#' @param echo       logical: if \code{TRUE}, each expression is printed after
#'                   parsing, before evaluation.
#' @param max.length a numeric value indicating the maximal number of characters
#'                   output for the deparse of a single expression.
#' @param spaced     logical: if \code{TRUE}, empty line is printed before each
#'                   expression.
#' @param check      logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{script.close}}, \code{\link{script.new}},
#' \code{\link{script.save}}, \code{\link{setsource}}
#'
#' @references
#' Ushey, K., Allaire, J., Wickham, H., & Ritchie, G. (2022). rstudioapi: Safely
#' access the RStudio API. R package version 0.14.
#' https://CRAN.R-project.org/package=rstudioapi
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Open  R script file
#' script.open("script.R")
#'
#' # Open  R script file and run the code
#' script.open("script.R", run = TRUE)
#' }
script.open <- function(path, line = 1, col = 1, cursor = TRUE, run = FALSE,
                        echo = TRUE, max.length = 999, spaced = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'path' is missing
  if (isTRUE(missing(path))) { stop("Please specify a character string for the argument 'path'.", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(path))) { stop("Input specified for the argument 'path' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'path'
    if (isTRUE(!is.character(path))) { stop("Please specify a character vector for the argument 'path'.", call. = FALSE) }

    # Check input 'line'
    if (isTRUE(line %% 1L != 0L || line < 0L)) { stop("Specify a positive integer number for the argument 'line'.", call. = FALSE) }

    # Check input 'col'
    if (isTRUE(col %% 1L != 0L || col < 0L)) { stop("Specify a positive integer number for the argument 'col'.", call. = FALSE) }

    # Check input 'cursor'
    if (isTRUE(!is.logical(cursor))) { stop("Please specify TRUE or FALSE for the argument 'cursor'.", call. = FALSE) }

    # Check input 'run'
    if (isTRUE(!is.logical(run))) { stop("Please specify TRUE or FALSE for the argument 'run'.", call. = FALSE) }

    # Check input 'echo'
    if (isTRUE(!is.logical(echo))) { stop("Please specify TRUE or FALSE for the argument 'echo'.", call. = FALSE) }

    # Check input 'max.length'
    if (isTRUE(max.length %% 1L != 0L || max.length < 0L)) { stop("Specify a positive integer number for the argument 'max.length'.", call. = FALSE) }

    # Check input 'spaced'
    if (isTRUE(!is.logical(spaced))) { stop("Please specify TRUE or FALSE for the argument 'spaced'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Package --------------------------------------------------------------------

  # rstudioapi package
  if (isTRUE(!nzchar(system.file(package = "rstudioapi")))) { stop("Package \"rstudioapi\" is needed for this function, please install the package.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Open R script
  invisible(rstudioapi::documentOpen(path = path, line = line, col = col,
                                     moveCursor = cursor))

  # Run R script
  if (isTRUE(run)) {

    source(path, echo = echo, spaced = spaced, max.deparse.length = max.length)

  }

}
