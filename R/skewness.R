#' Skewness
#'
#' This function computes the skewness.
#'
#' The same method for estimating skewness is used in SAS and SPSS. Missing values (\code{NA})
#' are stripped before the computation. Note that at least 3 observations are needed to compute
#' skewness.
#'
#' @param x           a numeric vector.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{kurtosis}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @return
#' Returns the estimated skewness of \code{x}.
#'
#' @export
#'
#' @examples
#' # Set seed of the random number generation
#' set.seed(123)
#' # Generate random numbers according to N(0, 1)
#' x <- rnorm(100)
#'
#' # Compute skewness
#' skewness(x)
skewness <- function(x, as.na = NULL, check = TRUE) {

  ####################################################################################
  # Input check

  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a numeric vector for the argument 'x'", call. = FALSE)

  }

  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #.......
    # Check input 'x': Missing
    if (all(is.na(x))) {

      stop("Vector specified in the argument 'x' is  is completely missing.", call. = FALSE)

    }

    #.......
    # Check input 'x': Numeric vector
    if (!is.numeric(x)) {

      stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE)

    }

    #.......
    # Check input 'x': Constant
    if (length(x) > 1) {

      if (var(x, na.rm = TRUE) == 0) {

        stop("Vector specified in the argument 'x' is a constant.", call. = FALSE)

      }

    }

  }

  ####################################################################################
  # Data

  # Convert user-missing values into NA
  if (!is.null(as.na)) {

    x <- misty::as.na(x, na = as.na)

    # Variable with missing values only
    if (all(is.na(x))) {

      stop("After converting user-missing values into NA, variable 'x' is completely missing.", call. = FALSE)

    }

    # Constant variables
    if (var(x, na.rm = TRUE) == 0) {

      stop("After converting user-missing values into NA, variable 'x' is a constant.", call. = FALSE)

    }

  }

  # Omit missing values
  if (any(is.na(x))) {

    x <- na.omit(x)

  }

  ####################################################################################
  # Main Function

  n <- length(x)

  if (n >= 3) {

    object <- (mean((x - mean(x))^3) / mean((x - mean(x))^2)^(3/2)) * sqrt(n * (n - 1)) / (n - 2)

  } else {

    warning("At least 3 observations are needed to compute skewness.", call. = FALSE)

    object <- NA

  }

  ####################################################################################
  # Return object

  return(object)

}
