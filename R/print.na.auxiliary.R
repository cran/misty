#' Print na.auxiliary object
#'
#' This function prints the \code{na.auxiliary} object
#'
#' @param x           \code{na.auxiliary} object.
#' @param tri         a character string indicating which triangular of the matrix to show on the console,
#'                    i.e., \code{both} for upper and lower triangular, \code{lower} for the lower triangular,
#'                    and \code{upper} for the upper triangular.
#' @param digits      integer value indicating the number of decimal places digits to be used for displaying
#'                    correlation coefficients and Cohen's d estimates.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{na.auxiliary}}
#'
#' @method print na.auxiliary
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(1, NA, 2, 5, 3, NA, 5, 2),
#'                   x2 = c(4, 2, 5, 1, 5, 3, 4, 5),
#'                   x3 = c(NA, 3, 2, 4, 5, 6, NA, 2),
#'                   x4 = c(5, 6, 3, NA, NA, 4, 6, NA))
#'
#' # Auxiliary variables
#' dat.na.auxiliary <- na.auxiliary(dat, output = FALSE)
#'
#' # Print na.auxiliary object with 3 digits
#' print(dat.na.auxiliary, digits = 3)
print.na.auxiliary <- function(x, tri = x$args$tri, digits = x$args$digits, check = TRUE, ...) {

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
    # Check digits argument
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  # Print object
  print.object <- x$result

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # Format correlation matrix
  print.object$cor <- apply(print.object$cor, 2, function(y) formatC(as.numeric(y), digits = digits, format = "f"))

  # Lower and/or upper triangular
  if (tri == "lower") {

    print.object$cor[upper.tri(print.object$cor)] <- ""

  }

  if (tri == "upper") {

    print.object$cor[lower.tri(print.object$cor)] <- ""

  }

  diag(print.object$cor) <- ""

  #-----------------------------------------
  # Format Cohen's d matrix

  print.object$d <- apply(print.object$d, 2, function(y) formatC(as.numeric(y), digits = digits, format = "f"))
  diag(print.object$d) <- ""

  # Print table
  print.object  <- data.frame(cbind(c("", colnames(print.object$cor), "", colnames(print.object$cor)),
                              rbind(colnames(print.object$cor), print.object$cor, colnames(print.object$cor), print.object$d)),
                              stringsAsFactors = FALSE)

  # Format
  print.object[, 1] <- paste0("   ", print.object[, 1])

  print.object[, 1] <- format(print.object[, 1], justify = "left")

  print.object[, -1] <- apply(print.object[, -1], 2, function(y) format(y, justify = "right"))

  ####################################################################################
  # Output

  cat(" Auxiliary Variables\n\n",
      " Variables related to the incomplete variable\n\n",
      "  Pearson product-moment correlation matrix\n")

  write.table(print.object[1:(nrow(x$result$cor) + 1), ], quote = FALSE, row.names = FALSE, col.names = FALSE)

  cat("\n")

  cat("  Variables related to the probability of missigness\n\n",
      "  Cohen's d\n")

  write.table(print.object[(nrow(x$result$cor) + 2):nrow(print.object), ], quote = FALSE, row.names = FALSE, col.names = FALSE)

  cat("\n",
     " Note. Indicator variables are in the rows (0 = miss, 1 = obs)\n")

}
