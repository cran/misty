#' Print cont.coef object
#'
#' This function prints the \code{cont.coef} object
#'
#' @param x           \code{cont.coef} object.
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
#' \code{\link{cont.coef}}
#'
#' @method print cont.coef
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(1, 1, 2, 1, 3, 3, 2, 2, 1, 2),
#'                   y = c(3, 2, 3, 1, 2, 4, 1, 2, 3, 4),
#'                   z = c(2, 2, 2, 1, 2, 2, 1, 2, 1, 2))
#'
#' # Adjusted Contingency coefficient between x and y
#' dat.cont1 <- cont.coef(dat[, c("x", "y")], output = FALSE)
#'
#' # Print cont.coef object with 5 digits
#' print(dat.cont1, digits = 5)
print.cont.coef <- function(x, tri = x$args$tri, digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
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

      cat("Adjusted Contingency Coefficient\n\n")

    } else {

      cat("Contingency Coefficient\n\n")

    }

  } else {

    if (isTRUE(x$args$adjust)) {

      cat("Adjusted Contingency Coefficient Matrix\n\n")

    } else {

      cat("Contingency Coefficient Matrix\n\n")

    }

  }

  if (is.null(dim(x$result))) {

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  } else {

    print(print.object, quote = FALSE, right = TRUE)

  }

}
