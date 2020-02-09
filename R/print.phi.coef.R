#' Print phi.coef object
#'
#' This function prints the \code{phi.coef} object
#'
#' @param x           \code{phi.coef} object.
#' @param tri         a character string or character vector indicating which triangular of the matrix
#'                    to show on the console, i.e., \code{both} for upper and lower triangular,
#'                    \code{lower} for the lower triangular, and \code{upper} for the upper
#'                    triangular.
#' @param digits      an integer value indicating the number of decimal places digits to be used for
#'                    displaying contingency coefficients.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{phi.coef}}
#'
#' @method print phi.coef
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0),
#'                   x2 = c(0, 1, 0, 0, 1, 1, 1, 1, 1, 1),
#'                   x3 = c(0, 1, 0, 1, 1, 1, 1, 1, 0, 0))
#'
#' # Ajusted phi coefficient between x1 and x2
#' dat.phi <- phi.coef(dat[, c("x1", "x2")], output = FALSE)
#'
#' # Print phi.coef object with 5 digits
#' print(dat.phi, digits = 5)
print.phi.coef <- function(x, tri = x$args$tri, digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    # Check input 'tri'
    if (any(!tri %in% c("both", "lower", "upper"))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
              call. = FALSE)

    }

    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  # Print object
  print.object <- x$result

  # Print triangular
  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  ####################################################################################
  # Main Function

  #----------------------------------------
  # Two variables
  if (is.null(dim(print.object))) {

    print.object <- cbind("  Estimate: ", formatC(print.object, digits = digits, format = "f"))

  #----------------------------------------
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

    # Set diagonal to "
    diag(print.object) <- ""

    # Format
    row.names(print.object) <- paste("", row.names(print.object))

  }

  ####################################################################################
  # Output

  if (is.null(dim(x$result))) {

    if (isTRUE(x$args$adjust)) {

      cat("Adjusted Phi Coefficient\n\n")

    } else {

      cat("Phi Coefficient\n\n")

    }

  } else {

    if (isTRUE(x$args$adjust)) {

      cat("Adjusted Phi Coefficient Matrix\n\n")

    } else {

      cat("Phi Coefficient Matrix\n\n")

    }

  }

  if (is.null(dim(x$result))) {

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  } else {

    print(print.object, quote = FALSE, right = TRUE)

  }

}
