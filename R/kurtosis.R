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

  ####################################################################################
  # Input check

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE)

  }

  #.......
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #----------------------------------------

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
    # Check input 'x': Zero variance
    if (length(x) > 1) {

      if (length(na.omit(unique(x))) == 1) {

        stop("Vector specified in the argument 'x' has zero variance.", call. = FALSE)

      }

    }

  }

  ####################################################################################
  # Data

  # Convert user-missing values into NA
  if (!is.null(as.na)) {

    x <- misty::as.na(x, as.na = as.na, check = check)

    # Variable with missing values only
    if (all(is.na(x))) {

      stop("After converting user-missing values into NA, variable 'x' is completely missing.", call. = FALSE)

    }

    # Zero variance
    if (length(na.omit(unique(x))) == 1) {

      stop("After converting user-missing values into NA, variable 'x' has zero variance.", call. = FALSE)

    }

  }

  # Omit missing values
  if (any(is.na(x))) {

    x <- na.omit(x)

  }

  ####################################################################################
  # Main Function

  n <- length(x)

  if (n >= 4) {

    m <- n * sum((x - mean(x))^4) / (sum((x - mean(x))^2)^2)

    object <- ((n + 1) * (m - 3) + 6) * (n - 1) / ((n - 2) * (n - 3))

    object <- ifelse(is.nan(object), NA, object )

  } else {

    warning("At least 4 observations are needed to compute excess kurtosis.", call. = FALSE)

    object <- NA

  }

  ####################################################################################
  # Return object

  return(object)

}
