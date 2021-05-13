#' Variance-Covariance Coverage
#'
#' This function computes the proportion of cases that contributes for the calculation of each variance and covariance.
#'
#' @param x           a matrix or data frame.
#' @param tri         a character string or character vector indicating which triangular of the matrix to show on the console,
#'                    i.e., \code{both} for upper and lower triangular, \code{lower} (default) for the lower triangular,
#'                    and \code{upper} for the upper triangular.
#' @param digits      an integer value indicating the number of decimal places to be used for displaying proportions.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.auxiliary}}, \code{\link{na.descript}},
#' \code{\link{na.indicator}}, \code{\link{na.pattern}}, \code{\link{na.prop}}.
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576. https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.). Chapman & Hall.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#' function call (\code{call}), type of analysis \code{type}, matrix or data frame specified in
#' \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(1, NA, NA, 6, 3),
#'                   y = c(7, NA, 8, 9, NA),
#'                   z = c(2, NA, 3, NA, 5))
#'
#' # Create missing data indicator matrix R
#' na.coverage(dat)
na.coverage <- function(x, tri = c("both", "lower", "upper"), digits = 2, as.na = NULL,
                        check = TRUE, output = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a matrix or data frame for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #.........................
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #.........................................

  if (isTRUE(check)) {

    #......
    # Check input 'tri'
    if (isTRUE(any(!tri %in% c("both", "lower", "upper")))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
           call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

      stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------------------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

  }

  #----------------------------------------
  # Print triangular
  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #----------------------------------------
  # As data frame

  df <- as.data.frame(x, stringsAsFactors = FALSE)

  ####################################################################################
  # Main Function

  # Pairwise combination
  comb.pair <- data.frame(combn(ncol(df), m = 2L), stringsAsFactors = FALSE)

  # Compute pairwise coverage
  cov.coverage <- vapply(comb.pair, function(y) nrow(na.omit(df[, c(y[1L], y[2L])])) / nrow(df), FUN.VALUE = double(1L))

  # Coverage matrix
  restab <- matrix(NA, ncol = ncol(x), nrow = (ncol(x)), dimnames = list(colnames(df), colnames(df)))

  # Assign coverage to lower triangular
  restab[lower.tri(restab)] <- cov.coverage

  # Copy lower triangular to upper triangular
  restab[upper.tri(restab)] <- t(restab)[upper.tri(restab)]

  # Variance coverage
  diag(restab) <- vapply(df, function(y) mean(!is.na(y)), FUN.VALUE = double(1L))

  ####################################################################################
  # Return object

  # Return object
  object <- list(call = match.call(),
                 type = "na.coverage",
                 data = x,
                 args = list(tri = tri, digits = digits, as.na = as.na, check = TRUE, output = output),
                 result = restab)

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
