#' Trim Whitespace from String
#'
#' This function removes whitespace from start and/or end of a string
#'
#' Note that this function is based on the \code{str_trim()} function from the \pkg{stringr} package by Hadley Wickham.
#'
#' @param x           a character vector.
#' @param side        a character string indicating the side on which to remove whitespace,
#'                    i.e., \code{"both"} (default), \code{"left"} or \code{"right"}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{mgsub}}, \code{\link{stromit}}
#'
#' @return
#' Returns a character vector with whitespaces removed from the vector specified in \code{x}.
#'
#' @references
#' Wickham, H. (2019). \emph{stringr: Simple, consistent wrappers for common string operations}.
#' R package version 1.4.0. \url{https://CRAN.R-project.org/package=stringr}
#'
#' @export
#'
#' @examples
#' x <- "  string  "
#'
#' # Remove whitespace at both sides
#' trim(x)
#'
#' # Remove whitespace at the left side
#' trim(x, side = "left")
#'
#' # Remove whitespace at the right side
#' trim(x, side = "right")
trim <- function(x, side = c("both", "left", "right"), check = TRUE) {

  #-----------------------------------------------------------------------------------
  #  Argument

  # Argument perc
  if (all(c("both", "left", "right") %in% side)) { side <- "both" }

  #-----------------------------------------------------------------------------------
  # Input check

  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a character vector for the argument 'x'", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (is.null(x)) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  if (isTRUE(check)) {

    # Check input 'x'
    if (!is.character(x)) {

      stop("Please specify a character vector for the argument 'x'", call. = FALSE)

    }

    # Check input 'side'
    if (length(side) != 1L) {

      stop("Please specify \"both\", \"left\", or \"right\ for the argument 'side'.", call. = FALSE)

    }

    # Check input 'side'
    if (!side %in% c("both", "left", "right")) {

      stop("Character string in the argument 'side' does not match with \"both\", \"left\", or \"right\".",
              call. = FALSE)

    }

  }

  #--------------------------------------------------------------------------------
  # Main Function

  object <- switch(side,
                   left  = sub("^\\s+", "", x),
                   right = sub("\\s+$", "", x),
                   both  = gsub("^\\s+|\\s+$", "", x))

  return(object)

}
