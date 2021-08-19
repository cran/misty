#' Skewness
#'
#' This function computes the skewness.
#'
#' The same method for estimating skewness is used in SAS and SPSS. Missing values
#' (\code{NA}) are stripped before the computation. Note that at least 3 observations
#' are needed to compute skewness.
#'
#' @param x           a numeric vector.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{kurtosis}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. New York: John Wiley & Sons.
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

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a numeric vector for the argument 'x'", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1) {

    stop("More than one variable specified for the argument 'x'.",call. = FALSE)

  }

  #......
  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  #-----------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  if (isTRUE(check)) {

    #......
    # Check if input 'x' is missing
    if (isTRUE(all(is.na(x)))) {

      stop("Vector specified in the argument 'x' is is completely missing.", call. = FALSE)

    }

    #......
    # Numeric vector for the argument 'x'?
    if (isTRUE(mode(x) != "numeric")) {

      stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE)

    }

    #.......
    # Check input 'x': Yero variance
    if (isTRUE(length(na.omit(unique(x))) == 1L)) {

        stop("Vector specified in the argument 'x' has zero variance.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data

  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na)

    # Variable with missing values only
    if (isTRUE(all(is.na(x)))) {

      stop("After converting user-missing values into NA, variable 'x' is completely missing.", call. = FALSE)

    }

    # Zero variance
    if (isTRUE(length(na.omit(unique(x))) == 1L)) {

      stop("After converting user-missing values into NA, variable 'x' has zero variance.", call. = FALSE)

    }

  }

  # Omit missing values
  if (isTRUE(any(is.na(x)))) {

    x <- na.omit(x)

  }

  ####################################################################################
  # Main Function

  n <- length(x)

  if (isTRUE(n >= 3L)) {

    object <- (mean((x - mean(x))^3L) / mean((x - mean(x))^2)^(3/2)) * sqrt(n * (n - 1L)) / (n - 2L)

    object <- ifelse(is.nan(object), NA, object )

  } else {

    warning("At least 3 observations are needed to compute skewness.", call. = FALSE)

    object <- NA

  }

  ####################################################################################
  # Return object

  return(object)

}
