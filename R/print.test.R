#' Print test object
#'
#' This function prints the \code{test} object
#'
#' @param x         \code{test} object.
#' @param digits    an integer value indicating the number of decimal places to be used for
#'                  displaying results.
#' @param p.digits  an integer value indicating the number of decimal places to be used for
#'                  displaying the \emph{p}-value.
#' @param check     logical: if \code{TRUE}, argument specification is checked.
#' @param ...       further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{levenes.test}}
#'
#' @method print test
#'
#' @export
#'
#' @examples
#' dat <- data.frame(y = c(2, 1, 4, 5, 3, 7, 8, 4, 1),
#'                   group = c(1, 1, 1, 2, 2, 2, 3, 3, 3))
#'
#' # Levene's test based on the median
#' dat.levene <- levenes.test(y ~ group, data = dat, output = FALSE)
#'
#' # Print test object with 5 digits for the p-value
#' print(dat.levene, p.digits = 5)
print.test <- function(x, digits = x$args$digits, p.digits = x$args$p.digits, check = TRUE, ...) {

  #----------------------------------------
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'digits'
    if (digits %% 1 != 0L || digits < 0L) {

      warning("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

    #......
    # Check input 'p.digits'
    if (p.digits %% 1L != 0L || p.digits < 0L) {

      warning("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  # Print object
  print.object <- x$result

  ####################################################################################
  # Coefficient Alpha
  if (x$type == "levene") {

    #-----------------------------------------------------------------------------------
    # Main Function

    #.....................................
    # Round

    print.object[, "Sum Sq"] <- formatC(print.object[, "Sum Sq"], digits = digits, format = "f")
    print.object[, "Mean Sq"] <- formatC(print.object[, "Mean Sq"], digits = digits, format = "f")
    print.object[1, "F value"] <- formatC(print.object[1, "F value"], digits = digits, format = "f")
    print.object[1, "Pr(>F)"] <- formatC(print.object[1, "Pr(>F)"], digits = p.digits, format = "f")

    #.....................................
    # Format

    print.object <- rbind(c("Df", "Sum Sq", "Mean Sq", "F", "p-value"), print.object)
    print.object <- cbind(c("", "  Group", "  Residuals"), print.object)

    print.object[3L, c("F value", "Pr(>F)")] <- ""

    print.object[, -1L] <- apply(print.object[, -1L], 2, format, justify = "right")
    print.object[, 1L] <- format(print.object[, 1L], justify = "left")

    #.....................................
    # Print output

    cat(" Levene's Test based on the", switch(x$args$method, median = "Median\n\n",
                                                             mean = "Arithmetic Mean\n\n"))

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE, na = "")

  }

}
