#' Print na.coverage
#'
#' This function prints the \code{na.coverage} object
#'
#' @param x          \code{na.coverage} object.
#' @param tri        a character string or character vector indicating which triangular of the matrix to show on the console,
#'                   i.e., \code{both} for upper and lower triangular, \code{lower} (default) for the lower triangular,
#'                   and \code{upper} for the upper triangular.
#' @param digits     an integer value indicating the number of decimal places to be used for displaying proportions.
#' @param check      logical: if \code{TRUE}, argument specification is checked.
#' @param ...        further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{na.coverage}}
#'
#' @method print na.coverage
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Matrix
#' mat <- matrix(c(1, NA, NA, 6, 3,
#'                 7, NA, 8, 9, NA,
#'                 2, NA, 3, NA, 5), ncol = 3)
#'
#' # Create missing data indicator matrix R
#' mat.coverage <- na.coverage(mat, output = FALSE)
#'
#' # Print na.coverage object with 5 digits
#' print(mat.coverage, digits = 5)
#'
#' #--------------------------------------
#' # Data frame
#' dat <- data.frame(x = c(1, NA, NA, 6, 3),
#'                   y = c(7, NA, 8, 9, NA),
#'                   z = c(2, NA, 3, NA, 5))
#'
#' # Create missing data indicator matrix R
#' na.coverage(dat)
#'
#' # Create missing data indicator matrix R
#' dat.coverage <- na.coverage(mat, output = FALSE)
#'
#' # Print na.coverage object with 5 digits
#' print(dat.coverage, digits = 5)
print.na.coverage <- function(x, tri = x$args$tri, digits = x$args$digits, check = TRUE, ...) {

  #-----------------------------------------------------------------------------------
  # Input check

  if (isTRUE(check)) {

    # Check input 'tri'
    if (any(!tri %in% c("both", "lower", "upper"))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
           call. = FALSE)

    }

    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer value for the argument 'digits'", call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------------
  # Main Function

  # Print object
  print.object <- x$result

  #........................................
  # Lower and/or upper triangular

  if (tri == "lower") {

    print.object[upper.tri(print.object)] <- ""

  }

  if (tri == "upper") {

    print.object[lower.tri(print.object)] <- ""

  }

  #-----------------------------------------------------------------------------------
  # Main Function

  # Format proportions
  print.object <- apply(print.object, 2, function(y) ifelse(!is.na(as.numeric(y)), formatC(as.numeric(y), digits = digits, format = "f"), ""))

  # Row names
  row.names(print.object) <- paste0("  ", row.names(x$result))

  #-----------------------------------------------------------------------------------
  # Output

  cat(" Variance-Covariance Coverage\n\n")

  print(print.object,  quote = FALSE, row.names = FALSE, max = 99999, right = TRUE)

}
