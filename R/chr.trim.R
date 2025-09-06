#' Trim Whitespace from String
#'
#' This function removes whitespace from start and/or end of a string
#'
#' @param x     a character vector.
#' @param side  a character string indicating the side on which to remove whitespace,
#'              i.e., \code{"both"} (default), \code{"left"} or \code{"right"}.
#' @param check logical: if \code{TRUE} (default), argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{chr.color}}, \code{\link{chr.grep}}, \code{\link{chr.grepl}},
#' \code{\link{chr.gsub}}, \code{\link{chr.omit}}, \code{\link{chr.trunc}}
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
#' # Example 1: Remove whitespace at both sides
#' chr.trim(x)
#'
#' # Example 2: Remove whitespace at the left side
#' chr.trim(x, side = "left")
#'
#' # Example 3: Remove whitespace at the right side
#' chr.trim(x, side = "right")
chr.trim <- function(x, side = c("both", "left", "right"), check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) || is.null(x))) { stop("Please specify a character vector for the argument 'x'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'x' and 'side'
  .check.input(s.character = list(side = c("both", "left", "right")), envir = environment(), input.check = check)

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

#_______________________________________________________________________________
