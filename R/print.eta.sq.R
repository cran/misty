#' Print eta.sq object
#'
#' This function prints the \code{eta.sq} object
#'
#' @param x           \code{eta.sq} object.
#' @param digits      an integer value indicating the number of decimal places to be used for displaying
#'                    eta squared.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at},
#'
#' @seealso
#' \code{\link{eta.sq}}
#'
#' @method print eta.sq
#'
#' @export
#' @examples
#' dat <- data.frame(x1 = c(1, 1, 1, 1, 2, 2, 2, 2, 2),
#'                   x2 = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                   y1 = c(3, 2, 4, 5, 6, 4, 7, 5, 7),
#'                   y2 = c(2, 4, 1, 5, 3, 3, 4, 6, 7))
#'
#' # Eta squared for y1 explained by x1
#' dat.eta.sq <- eta.sq(dat$y1, group = dat$x1, output = FALSE)
#'
#' # Print eta.sq object with 5 digits
#' print(dat.eta.sq, digits = 5)
print.eta.sq <- function(x, digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Print object
  print.object <- x$result

  #-----------------------------------------
  # Number of dependent variables, number of independent variables

  print.object.nrow <- ncol(x$dat$x) == 1
  print.object.ncol <- is.null(dim(x$dat$group))

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # One dependent variable, one independent variable

  if (print.object.nrow && print.object.ncol) {

    # Print object
    print.object <- cbind("  Estimate  ", formatC(print.object, digits = digits, format = "f"))

  } else {

    #-----------------------------------------
    # More than one dependent variable, more than one independent variable

    if (!print.object.nrow && !print.object.ncol) {

      # Variable names and format digis
      print.object <- rbind(c("", "Outcome", rep("", times = ncol(print.object) - 1)),
                            c("Group", colnames(print.object)),
                            cbind(rownames(print.object),
                                  formatC(print.object, digits = digits, format = "f")))

      # Format
      print.object[-c(1, 2), 1] <- paste("", print.object[-c(1, 2), 1])
      print.object[, 1] <- format(print.object[, 1], justify = "left")

      print.object[-1, 2] <- paste("", print.object[-1, 2])

      print.object[1, 2] <- format(print.object[1, 2], justify = "left", width = max(nchar(print.object[, 2])) )
      print.object[-1, 2] <- format(print.object[-1, 2], justify = "right")

      print.object[-1, -1] <- apply(print.object[-1, -1], 2, function(y) format(y, justify = "right"))

    }

    #-----------------------------------------
    # More than one dependent variable, one independent variable

    if (print.object.nrow && !print.object.ncol) {

      # Variable names and format digis
      print.object <- rbind(colnames(print.object),
                            formatC(print.object, digits = digits, format = "f"))

      # Format
      print.object <- format(print.object, justify = "right")

    }

    #-----------------------------------------
    # One dependent variable, more than one independent variable

    if (!print.object.nrow && print.object.ncol) {

      # Variable names and format digis
      print.object <- cbind(rownames(print.object),
                            formatC(print.object, digits = digits, format = "f"))

      # Format
      print.object <- format(print.object, justify = "right")

    }

  }

  ####################################################################################
  # Output

  if (print.object.nrow && print.object.ncol) {

      cat("Eta Squared\n\n")

  } else {

    cat("Eta Squared Matrix\n\n")

  }

  write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

}
