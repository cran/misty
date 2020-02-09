#' Print poly.cor object
#'
#' This function prints the \code{poly.cor} object
#'
#' @param x           \code{poly.cor} object.
#' @param tri         a character string indicating which triangular of the matrix to show on the console, i.e.,
#'                    \code{both} for upper and lower triangular, \code{lower} (default) for the lower triangular,
#'                    and \code{upper} for the upper triangular.
#' @param digits      an integer value indicating the number of decimal places to be used for
#'                    displaying correlation coefficients.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{poly.cor}}
#'
#' @method print poly.cor
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(1, 1, 3, 2, 1, 2, 3, 2, 3, 1),
#'                   x2 = c(1, 2, 1, 1, 2, 2, 2, 1, 3, 1),
#'                   x3 = c(1, 3, 2, 3, 3, 1, 3, 2, 1, 2))
#'
#' # Polychoric correlation matrix
#' dat.poly <- poly.cor(dat, output = FALSE)
#'
#' # Print poly.cor object with 3 digits
#' print(dat.poly, digits = 3)
print.poly.cor <- function(x, tri = x$args$tri, digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
  # Input check

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

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #----------------------------------------
  # Print object

  print.object <- x$result$cor

  #----------------------------------------
  # Print triangular

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  ####################################################################################
  # Main Function

  #----------------------------------------
  # Print object

  print.object <- formatC(print.object, digits = digits, format = "f")
  row.names(print.object) <- paste("", row.names(print.object))


  # Empty matrix diagonal
  diag(print.object) <- ""

  #----------------------------------------
  # Lower and/or upper triangular

  if (tri == "lower") {

    print.object[upper.tri(print.object)] <- ""

  }

  if (tri == "upper") {

    print.object[lower.tri(print.object)] <- ""

  }

  #----------------------------------------
  # Row names
  if (!is.null(rownames(print.object))) {

    row.names(print.object) <- format(row.names(print.object), justify = "left")

  }

  ####################################################################################
  # Output

  cat("Polychoric Correlation Matrix\n\n")

  print(print.object, quote = FALSE, right = TRUE, max = 99999)

}
