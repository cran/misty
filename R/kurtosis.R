#' Excess Kurtosis
#'
#' This function computes the excess kurtosis.
#'
#' The same method for estimating kurtosis is used in SAS and SPSS. Missing values (\code{NA})
#' are stripped before the computation. Note that at least 4 observations are needed to compute
#' excess kurtosis.
#'
#' @param x         a numeric vector.
#' @param as.na     a numeric vector indicating user-defined missing values,
#'                  i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check     logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{skewness}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @return
#' Returns the estimated excess kurtosis of \code{x}.
#'
#' @export
#'
#' @examples
#' # Set seed of the random number generation
#' set.seed(123)
#' # Generate random numbers according to N(0, 1)
#' x <- rnorm(100)
#'
#' # Compute excess kurtosis
#' kurtosis(x)
kurtosis <- function(x, as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1L) { stop("More than one variable specified for the argument 'x'.",call. = FALSE) }

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  if (isTRUE(check)) {

    # Check input 'x': Missing
    if (isTRUE(all(is.na(x)))) { stop("Vector specified in the argument 'x' is  is completely missing.", call. = FALSE) }

    # Check input 'x': Numeric vector
    if (isTRUE(mode(x) != "numeric")) { stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE) }

    #.......
    if (isTRUE(length(x) > 1L)) { if (isTRUE(length(na.omit(unique(x))) == 1L)) { stop("Vector specified in the argument 'x' has zero variance.", call. = FALSE) } }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    if (isTRUE(all(is.na(x)))) { stop("After converting user-missing values into NA, variable 'x' is completely missing.", call. = FALSE) }

    # Zero variance
    if (isTRUE(length(na.omit(unique(x))) == 1L)) { stop("After converting user-missing values into NA, variable 'x' has zero variance.", call. = FALSE) }

  }

  # Omit missing values
  if (isTRUE(any(is.na(x)))) {

    x <- na.omit(x)

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  n <- length(x)

  if (isTRUE(n >= 4L)) {

    m <- n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)

    object <- ((n + 1L) * (m - 3L) + 6L) * (n - 1L) / ((n - 2L) * (n - 3L))

    object <- ifelse(is.nan(object), NA, object )

  } else {

    warning("At least 4 observations are needed to compute excess kurtosis.", call. = FALSE)

    object <- NA

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
