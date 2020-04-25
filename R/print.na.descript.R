#' Print na.descript
#'
#' This function prints the \code{na.descript} object
#'
#' @param x           \code{na.descript} object.
#' @param table       logical: if \code{TRUE}, a frequency table with number of observed values (\code{"nObs"}),
#'                    percent of observed values (\code{"pObs"}), number of missing values (\code{"nNA"}), and
#'                    percent of missing values (\code{"pNA"}) is printed for each variable on the console.
#' @param digits      an integer value indicating the number of decimal places to be used for displaying percentages.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{na.descript}}
#'
#' @method print na.descript
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(1, NA, 2, 5, 3, NA, 5, 2),
#'                   x2 = c(4, 2, 5, 1, 5, 3, 4, 5),
#'                   x3 = c(NA, 3, 2, 4, 5, 6, NA, 2),
#'                   x4 = c(5, 6, 3, NA, NA, 4, 6, NA), stringsAsFactors = FALSE)
#'
#' # Descriptive statistics for missing data
#' dat.na.descript <- na.descript(dat, output = FALSE)
#'
#' # Print na.descript object with 3 digits
#' print(dat.na.descript, digits = 3)
print.na.descript <- function(x, table = x$args$table, digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
  # Input check

  if (isTRUE(check)) {

    #......
    # Check input 'table'
    if (!isTRUE(isTRUE(table) || !isTRUE(table))) {

      stop("Please specify TRUE or FALSE for the argument 'table'", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Please specify a positive integer value for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Data

  # Print object
  print.object <- x$res

  ####################################################################################
  # Main Function

  #----------------------------------------
  # Result table
  restab <- data.frame(statistic = c("No. of cases", "No. of complete cases", "No. of incomplete cases", "No. of values",
                                     "No. of observed values", "No. of missing values", "No. of variables",
                                     "Mean", "SD", "Minimum", "P25", "P75", "Maximum"),
                       no = c(print.object$no.cases, print.object$no.complete, print.object$no.incomplete,
                              print.object$no.values, print.object$no.observed.values, print.object$no.missing.values,
                              print.object$no.var, print.object$no.missing.mean, print.object$no.missing.sd,
                              print.object$no.missing.min, print.object$no.missing.p25, print.object$no.missing.p75, print.object$no.missing.max),
                       perc = c("", print.object$perc.complete, print.object$perc.incomplete, "", print.object$perc.observed.values,
                                print.object$perc.missing.values, "", print.object$perc.missing.mean, print.object$perc.missing.sd,
                                print.object$perc.missing.min, print.object$perc.missing.p25, print.object$perc.missing.p75, print.object$perc.missing.max),
                       stringsAsFactors = FALSE)

  #----------------------------------------
  # Format
  restab$statistic <- paste0("  ", restab$statistic)

  restab$statistic[8L:13L] <- paste0("  ", restab$statistic[8L:13L] )
  restab$statistic <- format(restab$statistic, justify = "left", width = max(nchar(restab$statistic)) + 1L)

  restab$no[8L:13L] <- format(formatC(as.numeric(restab$no[8L:13L]), digits = digits, format = "f"), justify = "right")
  restab$no[1L:7L] <- format(formatC(as.numeric(restab$no[1:7]), digits = 0L, format = "f"), justify = "right")
  restab$no <- format(restab$no, justify = "right")

  restab$perc[restab$perc != ""] <- paste0("(", formatC(as.numeric(restab$perc[restab$perc != ""]), digits = digits, format = "f"), "%)")

  restab$perc <- format(restab$perc, width = max(nchar(restab$perc)), justify = "right")

  ####################################################################################
  # Output

  cat(" Descriptive Statistics for Missing Data\n\n")

  write.table(restab[1L:3L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

  cat("\n")
  write.table(restab[4L:6L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

  cat("\n")
  write.table(restab[7L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)
  cat("  No. of missing values across all variables\n")
  write.table(restab[8L:13L, ], quote = FALSE, row.names = FALSE, col.names = FALSE)

  #----------------------------------------
  # Frequency table
  if (isTRUE(table)) {

    freqtab <- x$result$table.miss

    freqtab[, c("pObs", "pNA")] <- apply(freqtab[, c("pObs", "pNA")], 2, function(y) paste0(formatC(y, digits = digits, format = "f"), "%"))
    freqtab <- rbind(colnames(freqtab), freqtab)

    freqtab[, -1L] <- apply(freqtab[ -1L], 2, format, justify = "right")

    freqtab[, 1L] <- paste0("    ", format(freqtab[, 1L], justify = "left"))

    cat("\n")
    write.table(freqtab, quote = FALSE, row.names = FALSE, col.names = FALSE)

  }

}
