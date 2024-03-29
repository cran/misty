#' Skewness and Kurtosis
#'
#' The function \code{skewness} computes the skewness, the function \code{kurtosis}
#' computes the kurtosis.
#'
#' The same method for estimating skewness and kurtosis is used in SAS and SPSS.
#' Missing values (\code{NA}) are stripped before the computation. Note that at
#' least 3 observations are needed to compute skewness and at least 4 observations
#' are needed to compute excess kurtosis.
#'
#' @param ...   a numeric vector. Alternatively, an expression indicating the
#'              variable names in \code{data} e.g., \code{skewness(x1, data = dat)}.
#' @param data  a data frame when specifying the variable in the argument
#'              \code{...}. Note that the argument is \code{NULL} when specifying
#'              a numeric vector for the argument \code{...}.
#' @param as.na a numeric vector indicating user-defined missing values,
#'              i.e. these values are converted to \code{NA} before conducting
#'              the analysis.
#' @param check logical: if \code{TRUE} (default), argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{des.nascript}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. New York: John Wiley & Sons.
#'
#' @return
#' Returns the estimated skewness or kurtosis of \code{x}.
#'
#' @export
#'
#' @examples
#' # Set seed of the random number generation
#' set.seed(123)
#' # Generate random numbers according to N(0, 1)
#' x <- rnorm(100)
#'
#' # Example 1: Compute skewness
#' skewness(x)
#'
#' # Example 2: Compute excess kurtosis
#' kurtosis(x)
skewness <- function(..., data = NULL, as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data ####

  #...................
  ### Data using the argument 'data' ####
  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a numeric vector")

    # Extract variables
    x <- data[, var.names]

  #...................
  ### Data without using the argument 'data' ####
  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

  }

  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1L) { stop("More than one variable specified for the argument 'x'.", call. = FALSE) }

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  # Omit missing values
  if (isTRUE(any(is.na(x)))) {

    x <- na.omit(x)

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check if input 'x' is missing
    if (isTRUE(all(is.na(x)))) { stop("Vector specified in the argument 'x' is is completely missing.", call. = FALSE) }

    # Numeric vector for the argument 'x'?
    if (isTRUE(mode(x) != "numeric")) { stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE) }

    # Check input 'x': Yero variance
    if (isTRUE(length(na.omit(unique(x))) == 1L)) { stop("Vector specified in the argument 'x' has zero variance.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  n <- length(x)

  if (isTRUE(n >= 3L)) {

    object <- (mean((x - mean(x))^3L) / mean((x - mean(x))^2)^(3/2)) * sqrt(n * (n - 1L)) / (n - 2L)

    object <- ifelse(is.nan(object), NA, object )

  } else {

    warning("At least 3 observations are needed to compute skewness.", call. = FALSE)

    object <- NA

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(object)

}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#' @rdname kurtosis
kurtosis <- function(..., data = NULL, as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a numeric vector")

    # Extract variables
    x <- data[, var.names]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

  }

  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1L) { stop("More than one variable specified for the argument 'x'.", call. = FALSE) }

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

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
  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  # Omit missing values
  if (isTRUE(any(is.na(x)))) { x <- na.omit(x) }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  n <- length(x)

  if (isTRUE(n >= 4L)) {

    m <- n * sum((x - mean(x))^4L) / (sum((x - mean(x))^2)^2)

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
