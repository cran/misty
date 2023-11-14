#' Compute lagged or leading values of a vector
#'
#' This function computes lagged or leading values of a vector by a specified
#' number of observations. By default, the function returns lag-1 values of the
#' vector specified in the argument 'x'.
#'
#' @param x     a numeric vector.
#' @param n     a numeric value denoting the offset by which to lag (positive
#'              value) or lead (negative value), e.g. \code{n = 1} (default)
#'              returns lag-1 values, and  \code{n = -1} returns lead-1 values of
#'              the vector specified in 'x'.
#' @param fill  a numeric value or \code{NA} (default) used to pad \code{x} back
#'              to its original size after the lag or lead has been applied.
#' @param as.na a numeric vector indicating user-defined missing values, i.e.
#'              these values are converted to \code{NA} before computing lagged
#'              or leading values of the vector.
#' @param check logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Hadley Wickham, Romain Francois, Lionel Henry, and Kirill Müller, and Davis Vaughan.
#'
#' @seealso
#' \code{\link{center}}, \code{\link{rec}}, \code{\link{dummy.c}}, \code{\link{item.reverse}}.
#'
#' @references
#' Wickham H, Francois R, Henry L, Müller K, & Vaughan D (2023). \emph{dplyr: A Grammar of Data
#' Manipulation}. R package version 1.1.3, <https://CRAN.R-project.org/package=dplyr>.
#'
#' @return
#' Returns a numeric vector  with the same length containing lagged or leading values.
#'
#'
#' @note
#' This function is a modified copy of the \code{lag()} and \code{lead()} functions
#' in the \pkg{dplyr} package by Wickham et al. (2023).
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Lagged values
#'
#' # Lag-1 values
#' shift(1:10)
#'
#' # Lag-2 values
#' shift(1:10, n = 2)
#'
#' # Value -99 to pad 'x'
#' shift(1:10, fill = -99)
#'
#' #--------------------------------------
#' # Leading values
#'
#' # Lead-1 values
#' shift(1:10, n = -1)
#'
#' # Lead-2 values
#' shift(1:10, n = -2)
shift <- function(x, n = 1, fill = NA, as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with NAs
    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    if (isTRUE(all(is.na(x)))) { stop("After converting user-missing values into NA, variable 'x' is completely missing.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'x'
    if (isTRUE(inherits(x, "ts"))) { stop("Argument `x` is a time-series object, please use the function `stats::lag()`.", call. = FALSE) }

    # Check input 'n'
    if (isTRUE(n %% 1L != 0L)) { stop("Please specify an integer number for the argument 'n'.", call. = FALSE) }

    # Check input 'fill'
    if (isTRUE(length(fill) != 1L)) { stop("Please specify NA or a numeric value for the argument 'fill'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Lagged or leading values
  lag <- n >= 0L

  # Number of positions to lag or lead
  n <- abs(n)

  # Length of the vector 'x'
  size <- length(x)

  # Number of positions to lag or lead greater than the length of the vector 'x'
  if (isTRUE(n > size)) { n <- size }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Pad 'x' with NA ####

  if (isTRUE(is.na(fill))) {

    if (isTRUE(n != size)) {

      loc_fill <- rep(NA_integer_, n)

      if (isTRUE(lag)) {

        object <- x[c(loc_fill, seq(1L, size - n))]

      } else {

        object <- x[c(seq(1L + n, size), loc_fill)]

      }

    } else {

      object <- rep(NA_integer_, size)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Pad 'x' with user-specified value ####

  } else {

    if (isTRUE(n != size)) {

      fill <- rep(fill, n)

      if (isTRUE(lag)) {

        object <- c(fill, x[ seq(1L, size - n)])

      } else {

        object <- c(x[ seq(1L + n, size)], fill)

      }

    } else {

      object <- rep(fill, size)

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(object)

}
