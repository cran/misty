#' Print na.pattern
#'
#' This function prints the \code{na.pattern} object
#'
#' @param x           \code{na.pattern} object.
#' @param order       logical: if \code{TRUE}, variables are ordered from left to right in increasing order
#'                    of missing values.
#' @param digits      an integer value indicating the number of decimal places to be used for displaying percentages.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{na.pattern}}
#'
#' @method print na.pattern
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(1, NA, NA, 6, 3),
#'                   y = c(7, NA, 8, 9, NA),
#'                   z = c(2, NA, 3, NA, 5))
#'
#' # Compute a summary of missing data patterns
#' dat.pattern <- na.pattern(dat, output = FALSE)
#'
#' # Print na.pattern object with 5 digits
#' print(dat.pattern, digits = 5)
print.na.pattern <- function(x, order = x$args$order, digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
  # Input

  if (isTRUE(check)) {

    # Check order argument
    if (isFALSE(isTRUE(order) | isFALSE(order))) {

      stop("Please specify TRUE or FALSE for the argument 'order'.", call. = FALSE)

    }

    # Check digits argument
    if (digits %% 1 != 0 | digits < 0) {

      stop("Please specify a positive integer value for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Data

  # Print object
  print.object <- x$result

  ####################################################################################
  # Main Function

  # Percentages

  print.object[, "Perc"] <- paste0(formatC(as.numeric(print.object[, "Perc"]), digits = digits, format = "f"), "%")
  print.object[, "pNA"] <- paste0(formatC(as.numeric(print.object[, "pNA"]), digits = digits, format = "f"), "%")
  print.object[nrow(print.object), ncol(print.object)] <- ""

  # Format
  colnames(print.object)[1] <- " Pattern"

  ####################################################################################
  # Output

  cat(" Missing Data Pattern\n\n")

  print(print.object, row.names = FALSE, max = 99999, right = TRUE)

}




