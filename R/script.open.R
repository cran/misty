#' Open, Close and Save R Script in RStudio
#'
#' The function \code{script.open} opens an R script, R markdown script, or SQL
#' script in RStudio, the function \code{script.close} closes an R script, and
#' the function \code{script.save} saves an R script. Note that the R script need
#' to have a file location before the script can be saved.
#'
#' @param path       a character string indicating the path of the script.
#' @param line       a numeric value indicating the line in the script to navigate
#'                   to.
#' @param col        a numeric value indicating the column in the script to
#'                   navigate to.
#' @param cursor     logical: if \code{TRUE} (default), the cursor moves to the
#'                   requested location after opening the document.
#' @param run        logical: if \code{TRUE}, the code is executed after the
#'                   document is opened
#' @param echo       logical: if \code{TRUE} (default), each expression is printed
#'                   after parsing, before evaluation.
#' @param max.length a numeric value indicating the maximal number of characters
#'                   output for the deparse of a single expression.
#' @param spaced     logical: if \code{TRUE} (default), empty line is printed before
#'                   each expression.
#' @param save       logical: if \code{TRUE}, the script is saved before closing
#'                   when using the function \code{script.close}.
#' @param all        logical: if \code{TRUE}, all scripts opened in RStudio are
#'                   saved when using the function \code{script.save}.
#' @param check      logical: if \code{TRUE} (default), argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{script.new}}, \code{\link{script.save}}, \code{\link{script.copy}},
#' \code{\link{setsource}}
#'
#' @references
#' Ushey, K., Allaire, J., Wickham, H., & Ritchie, G. (2023). \emph{rstudioapi: Safely
#' access the RStudio API}. R package version 0.15.0
#' https://CRAN.R-project.org/package=rstudioapi
#'
#' @note
#' This function uses the \code{documentOpen()}, \code{documentPath()},
#' \code{documentClose()}, \code{documentSave()}, and \code{documentSaveAll()}
#' functions in the \pkg{rstudioapi} package by Kevin Ushey,  JJ Allaire, Hadley
#' Wickham, and Gary Ritchie (2023).
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Example 1: Open  R script file
#' script.open("script.R")
#'
#' # Example 2: Open  R script file and run the code
#' script.open("script.R", run = TRUE)
#'
#' # Example 3: Close current R script file
#' script.close()
#'
#' # Example 4: Save current R script
#' script.save()
#'
#' # Example 5: Save all R scripts
#' script.save(all = TRUE)
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

    # rstudioapi package
    if (isTRUE(!nzchar(system.file(package = "rstudioapi")))) { stop("Package \"rstudioapi\" is needed for this function, please install the package.", call. = FALSE) }

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
  # Main Function --------------------------------------------------------------

  # Open R script
  invisible(rstudioapi::documentOpen(path = path, line = line, col = col, moveCursor = cursor))

  # Run R script
  if (isTRUE(run)) {

    source(path, echo = echo, spaced = spaced, max.deparse.length = max.length)

  }

}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#' @rdname script.close
script.close <- function(save = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # rstudioapi package
    if (isTRUE(!nzchar(system.file(package = "rstudioapi")))) { stop("Package \"rstudioapi\" is needed for this function, please install the package.", call. = FALSE) }

    # Check input 'save'
    if (isTRUE(!is.logical(save))) { stop("Please specify TRUE or FALSE for the argument 'save'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  if (isTRUE(save)) {

    # Path of the current R script
    path.script <- try(dirname(rstudioapi::documentPath()), silent = TRUE)

    # Check
    if (isTRUE(class(path.script) == "try-error")) {

      stop("The current R script does not have a file location yet, please save the R script or set the argument 'save' to 'FALSE'.", call. = FALSE)

    }

  }

  # Close R script
  invisible(rstudioapi::documentClose(save = save))

}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#' @rdname script.save
script.save <- function(all = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'all'
    if (isTRUE(!is.logical(all))) { stop("Please specify TRUE or FALSE for the argument 'all'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Package --------------------------------------------------------------------

  # rstudioapi package
  if (isTRUE(!nzchar(system.file(package = "rstudioapi")))) { stop("Package \"rstudioapi\" is needed for this function, please install the package.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Path of the current R script
  path.script <- try(dirname(rstudioapi::documentPath()), silent = TRUE)

  # Check
  if (isTRUE(class(path.script) == "try-error")) {

    stop("The current R script does not have a file location yet, please save the R script first.", call. = FALSE)

  }

  if (isTRUE(all)) {

    # Save all R scripts
    invisible(rstudioapi::documentSaveAll())

  } else {

    # Save R script
    invisible(rstudioapi::documentSave())

  }

}
