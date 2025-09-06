#' Multiple Pattern Matching And Replacements
#'
#' This function is a multiple global string replacement wrapper that allows access
#' to multiple methods of specifying matches and replacements.
#'
#' @param pattern     a character vector with character strings to be matched.
#' @param replacement a character vector equal in length to \code{pattern} or of
#'                    length one which are a replacement for matched patterns.
#' @param x           a character vector where matches and replacements are sought.
#' @param recycle     logical: if \code{TRUE}, replacement is recycled if lengths differ.
#' @param check       logical: if \code{TRUE} (default), argument specification is
#'                    checked.
#' @param ...         additional arguments to pass to the \code{regexpr} or \code{sub}
#'                    function.
#' @author
#' Mark Ewing
#'
#' @seealso
#' \code{\link{chr.color}}, \code{\link{chr.grep}}, \code{\link{chr.grepl}},
#' \code{\link{chr.omit}}, \code{\link{chr.trim}}, \code{\link{chr.trunc}}
#'
#' @references
#' Mark Ewing (2019). \emph{mgsub: Safe, Multiple, Simultaneous String Substitution}.
#' R package version 1.7.1. https://CRAN.R-project.org/package=mgsub
#'
#' @return
#' Return a character vector of the same length and with the same attributes as
#' \code{x} (after possible coercion to character).
#'
#' @note
#' This function was adapted from the \code{mgsub()} function in the \pkg{mgsub}
#' package by Mark Ewing (2019).
#'
#' @export
#'
#' @examples
#' # Example 1: Replace 'the' and 'they' with 'a' and 'we'
#' chr.vector <- "they don't understand the value of what they seek."
#' chr.gsub(c("the", "they"), c("a", "we"), chr.vector)
#'
#' # Example 2: Replace 'heyy' and 'ho' with 'yo'
#' chr.vector <- c("hey ho, let's go!")
#' chr.gsub(c("hey", "ho"), "yo", chr.vector, recycle = TRUE)
#'
#' # Example 3: Replace with regular expressions
#' chr.vector <- "Dopazamine is not the same as dopachloride or dopastriamine, yet is still fake."
#' chr.gsub(c("[Dd]opa([^ ]*?mine)","fake"), c("Meta\\1","real"), chr.vector)
chr.gsub <- function(pattern, replacement, x, recycle = FALSE, check = TRUE, ...) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'pattern' is missing or NULL
  if (isTRUE(missing(pattern) || is.null(pattern))) { stop("Please specify a character vector for the argument 'pattern'", call. = FALSE) }

  # Check if input 'replacement' is missing or NULL
  if (isTRUE(missing(replacement) || is.null(replacement))) { stop("Please specify a character vector for the argument 'replacement'", call. = FALSE) }

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) || is.null(x))) { stop("Please specify a character vector for the argument 'x'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  # Logical vector with TRUE = not missing
  sna <- !is.na(x)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'recycle'
  .check.input(logical = "recycle", envir = environment(), input.check = check)

  # Check if arguments 'argument' and 'replacement' have the same length
  if (isTRUE(!recycle & length(pattern) != length(replacement))) { stop("Pattern and replacement vectors must be the same length if recycle = FALSE.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  if (isTRUE(length(replacement) > length(pattern))) {

    warning("More replacements than search strings provided, some will be dropped.", call. = FALSE)

    replacement <- replacement[seq_along(pattern)]

  }

  if (isTRUE(recycle && length(pattern) != length(replacement))) {

    lp <- length(pattern)
    lr <- length(replacement)
    replacement <- rep(replacement, ceiling(lp/lr))[seq_along(pattern)]

  }

  result <- vapply(X = x[sna], FUN = .worker, FUN.VALUE = c(""), USE.NAMES = FALSE, pattern = pattern, replacement = replacement, ...)

  x[sna] <- result

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(x)

}

#_______________________________________________________________________________
