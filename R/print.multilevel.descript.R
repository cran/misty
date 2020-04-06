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
#'                   x3 = c(2, 1, 2, 2, 3, 3, 5, 2, 4), stringsAsFactors = FALSE)
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
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Specify a positive integer value for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check icc.digits argument
    if (icc.digits %% 1L != 0L || icc.digits < 0L) {

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
  if (length(x$result$no.obs) == 1L) {

    # Format
    for (i in c(5L, 6L, 13L, 14L, 15L)) {

      print.object[i, 2L] <- formatC(as.numeric(unlist(print.object[i, 2L])), digits = digits, format = "f")

    }

    print.object[10L, 2L] <- formatC(as.numeric(unlist(print.object[10L, 2L])), digits = icc.digits, format = "f")
    print.object[11L, 2L] <- formatC(as.numeric(unlist(print.object[11L, 2L])), digits = icc.digits, format = "f")

    print.object[, 1L] <- paste("", print.object[, 1L])


    print.object[, 1L] <- format(print.object[, 1L, drop = FALSE])

    print.object[, 1L] <- format(unlist(print.object[, 1L]), justify = "left")
    print.object[, 2L] <- format(as.character(print.object[, 2L]), justify = "right")

  #-----------------------------------------
  # More than one variable
  } else {

    print.object <- rbind(c("", names(x$result$no.obs)), print.object)

    # Format
    for (i in c(6L, 7L, 14L, 15L, 16L)) {

      print.object[i, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[i, 2L:ncol(print.object)])), digits = digits, format = "f")

    }

    print.object[11L, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[11L, 2L:ncol(print.object)])), digits = icc.digits, format = "f")
    print.object[12L, 2L:ncol(print.object)] <- formatC(as.numeric(unlist(print.object[12L, 2L:ncol(print.object)])), digits = icc.digits, format = "f")

    print.object[, 1L] <- paste("", print.object[, 1L])


    print.object[, 1L] <- format(print.object[, 1L, drop = FALSE])

    print.object[, 1L] <- format(unlist(print.object[, 1L]), justify = "left")
    print.object[, 2L:ncol(print.object)] <- apply(print.object[, 2L:ncol(print.object)], 2, function(y) format(as.character(y), justify = "right"))

  }

  ####################################################################################
  # Output

  cat("Multilevel Descriptive Statistics\n\n")

  write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

}
