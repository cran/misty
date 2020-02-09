#' Print multilevel.descript object
#'
#' This function prints the \code{multilevel.descript} object
#'
#' @param x           \code{multilevel.descript} object.
#' @param digits      an integer value indicating the number of decimal places to be used.
#' @param icc.digits  an integer indicating the number of decimal places to be used for displaying
#'                    intraclass correlation coefficients.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{multilevel.descript}}
#'
#' @method print multilevel.descript
#'
#' @export
#'
#' @examples
#' dat <- data.frame(id = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
#'                   group = c(1, 1, 1, 1, 2, 2, 3, 3, 3),
#'                   x1 = c(2, 3, 2, 2, 1, 2, 3, 4, 2),
#'                   x2 = c(3, 2, 2, 1, 2, 1, 3, 2, 5),
#'                   x3 = c(2, 1, 2, 2, 3, 3, 5, 2, 4))
#'
#' # Multilevel descriptve statistics for x1
#' dat.ml.descript <- multilevel.descript(dat$x1, group = dat$group, output = FALSE)
#'
#' # Print multilevel.descript object with 5 digits for displaying ICC
#' print(dat.ml.descript, icc.digits = 5)
print.multilevel.descript <- function(x, digits = x$args$digits, icc.digits = x$args$icc.digits,
                                      check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check digits argument
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check icc.digits argument
    if (icc.digits %% 1 != 0 | icc.digits < 0) {

      stop("Specify a positive integer value for the argument 'icc.digits'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  print.object <- data.frame(cbind(c("No. of cases", "No. of missing values",
                                  "", "No. of groups", "Average group size", "SD group size", "Min group size", "Max group size",
                                  "", "ICC(1)", "ICC(2)",
                                  "", "Design effect", "Design effect sqrt", "Effective sample size"),
                                rbind(x$result$no.obs, x$result$no.miss,
                                      "", x$result$no.group, x$result$m.group.size, x$result$sd.group.size, x$result$min.group.size, x$result$max.group.size,
                                      "", x$result$icc1, x$result$icc2,
                                      "", x$result$deff, x$result$deff.sqrt, x$result$n.effect)),
                                stringsAsFactors = FALSE)

  #-----------------------------------------
  # One variable
  if (length(x$result$no.obs) == 1) {

    # Format
    for (i in c(5, 6, 13, 14, 15)) {

      print.object[i, 2] <- formatC(as.numeric(unlist(print.object[i, 2])), digits = digits, format = "f")

    }

    print.object[10, 2] <- formatC(as.numeric(unlist(print.object[10, 2])), digits = icc.digits, format = "f")
    print.object[11, 2] <- formatC(as.numeric(unlist(print.object[11, 2])), digits = icc.digits, format = "f")

    print.object[, 1] <- paste("", print.object[, 1])


    print.object[, 1] <- format(print.object[, 1, drop = FALSE])

    print.object[, 1] <- format(unlist(print.object[, 1]), justify = "left")
    print.object[, 2] <- format(as.character(print.object[, 2]), justify = "right")

  #-----------------------------------------
  # More than one variable
  } else {

    print.object <- rbind(c("", names(x$result$no.obs)), print.object)

    # Format
    for (i in c(6, 7, 14, 15, 16)) {

      print.object[i, 2:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[i, 2:ncol(print.object)])), digits = digits, format = "f")

    }

    print.object[11, 2:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[11, 2:ncol(print.object)])), digits = icc.digits, format = "f")
    print.object[12, 2:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[12, 2:ncol(print.object)])), digits = icc.digits, format = "f")

    print.object[, 1] <- paste("", print.object[, 1])


    print.object[, 1] <- format(print.object[, 1, drop = FALSE])

    print.object[, 1] <- format(unlist(print.object[, 1]), justify = "left")
    print.object[, 2:ncol(print.object)] <- apply(print.object[, 2:ncol(print.object)], 2, function(y) format(as.character(y), justify = "right"))

  }

  ####################################################################################
  # Output

  cat("Multilevel Descriptive Statistics\n\n")

  write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

}
