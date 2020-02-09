#' Print cramers.v object
#'
#' This function prints the \code{cramers.v} object
#'
#' @param x           \code{cramers.v} object.
#' @param tri         a character string or character vector indicating which triangular of the matrix
#'                    to show on the console, i.e., \code{both} for upper and lower triangular,
#'                    \code{lower} for the lower triangular, and \code{upper} for the upper
#'                    triangular.
#' @param digits      an integer value indicating the number of decimal places digits to be used for
#'                    displaying Cramer's V.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cramers.v}}
#'
#' @method print cramers.v
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(1, 1, 2, 1, 3, 3, 2, 2, 1, 2),
#'                   y = c(1, 2, 2, 1, 3, 4, 1, 2, 3, 1),
#'                   z = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 2))
#'
#' # Bias-corrected Cramer's V between x and y
#' dat.cramer <- cramers.v(dat[, c("x", "y")], output = FALSE)
#'
#' # Print cramers.v object with 4 digits
#' print(dat.cramer, digits = 4)
print.cramers.v <- function(x, tri = x$args$tri, digits = x$args$digits, check = TRUE, ...) {

  #-----------------------------------------------------------------------------------
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'tri'
    if (any(!tri %in% c("both", "lower", "upper"))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
              call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------------
  # Data and Arguments

  # Print object
  print.object <- x$result

  # Print triangular
  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #-----------------------------------------------------------------------------------
  # Main Function

  #........................................
  # Two variables
  if (is.null(dim(print.object))) {

    print.object <- cbind("  Estimate: ", formatC(print.object, digits = digits, format = "f"))

  #........................................
  # More than two variables
  } else {

    # Format contingenccy coefficients
    print.object <- formatC(print.object, digits = digits, format = "f")

    # Lower and/or upper triangular
    if (tri == "lower") {

      print.object[upper.tri(print.object)] <- ""

    }

    if (tri == "upper") {

      print.object[lower.tri(print.object)] <- ""

    }

    # Diagonal
    diag(print.object) <- ""

    # Format
    row.names(print.object) <- paste("", row.names(print.object))

  }

  #-----------------------------------------------------------------------------------
  # Output

  if (is.null(dim(x$result))) {

    if (isTRUE(x$args$correct)) {

      cat("Bias-Corrected Cramer's V\n\n")

    } else {

      cat("Cramer's V\n\n")

    }

  } else {

    if (isTRUE(x$args$correct)) {

      cat("Bias-Corrected Cramer's V Matrix\n\n")

    } else {

      cat("Cramer's V Matrix\n\n")

    }

  }

  if (is.null(dim(x$result))) {

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  } else {

    print(print.object, quote = FALSE, right = TRUE)

  }

}
