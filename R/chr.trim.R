#' Trim Whitespace from String
#'
#' This function removes whitespace from start and/or end of a string
#'
#' @param x     a character vector.
#' @param side  a character string indicating the side on which to remove whitespace,
#'              i.e., \code{"both"} (default), \code{"left"} or \code{"right"}.
#' @param check logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{chr.gsub}}, \code{\link{chr.omit}}
#'
#' @return
#' Returns a character vector with whitespaces removed from the vector specified
#' in \code{x}.
#'
#' @references
#' Wickham, H. (2019). \emph{stringr: Simple, consistent wrappers for common string
#' operations}.
#' R package version 1.4.0. \url{https://CRAN.R-project.org/package=stringr}
#'
#' @note
#' This function is based on the \code{str_trim()} function from the \pkg{stringr}
#' package by Hadley Wickham.
#'
#' @export
#'
#' @examples
#' x <- "  string  "
#'
#' # Remove whitespace at both sides
#' chr.trim(x)
#'
#' # Remove whitespace at the left side
#' chr.trim(x, side = "left")
#'
#' # Remove whitespace at the right side
#' chr.trim(x, side = "right")
chr.trim <- function(x, side = c("both", "left", "right"), check = TRUE) {

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'x'
  if (isTRUE(missing(x))) { stop("Please specify a character vector for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'x'
    if (isTRUE(!is.character(x))) { stop("Please specify a character vector for the argument 'x'", call. = FALSE) }

    # Check input 'side'
    if (isTRUE(!side %in% c("both", "left", "right"))) { stop("Character string in the argument 'side' does not match with \"both\", \"left\", or \"right\".", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Argument -------------------------------------------------------------------

  # Argument 'side'
  if (isTRUE(all(c("both", "left", "right") %in% side))) { side <- "both" }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  object <- switch(side,
                   left  = sub("^\\s+", "", x),
                   right = sub("\\s+$", "", x),
                   both  = gsub("^\\s+|\\s+$", "", x))

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
