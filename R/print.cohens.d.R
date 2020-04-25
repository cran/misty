#' Print cohens.d object
#'
#' This function prints the \code{cohens.d} object
#'
#' @param x           \code{cohens.d} object.
#' @param digits      an integer value indicating the number of decimal places to be used for
#'                    displaying results.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cohens.d}}
#'
#' @method print cohens.d
#'
#' @export
#' @examples
#' #--------------------------------------
#' # Between-subject design
#' dat.bs <- data.frame(group = c("cont", "cont", "cont", "treat",  "treat"),
#'                      y1 = c(1, 3, 2, 5, 7),
#'                      y2 = c(4, 3, 3, 6, 4),
#'                      y3 = c(7, 5, 7, 3, 2), stringsAsFactors = FALSE)
#'
#' # Standardized mean difference divided by the weighted pooled
#' # standard deviation without small sample correction factor
#' dat.bs.d <- cohens.d(y1 ~ group, data = dat.bs, output = FALSE)
#'
#' # Print cohens.d object with 5 digits
#' print(dat.bs.d, digits = 5)
#'
#' #--------------------------------------
#' # Within-subject design
#' dat.ws <- data.frame(pre = c(1, 3, 2, 5, 7),
#'                      post = c(2, 2, 1, 6, 8), stringsAsFactors = FALSE)
#'
#' # Standardized mean difference divided by the pooled
#' # standard deviation while controlling for the correlation
#' # without small sample correction factor
#' dat.ws.d <- cohens.d(post ~ pre, data = dat.ws, paired = TRUE, output = FALSE)
#'
#' # Print cohens.d object with 5 digits
#' print(dat.ws.d, digits = 5)
print.cohens.d <- function(x, digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isTRUE(check)) {

    # Check input 'digits'
    if (digits %% 1L != 0L | digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  # Print object
  print.object <- x$result

  ####################################################################################
  # Main Function

  #---------------------------------------
  # Between-subject design
  if (!isTRUE(x$args$paired)) {

    print.object[, !names(print.object) %in% c("variable", "n1", "n2", "nNA1", "nNA2")] <- vapply(print.object[,  !names(print.object) %in% c("variable", "n1", "n2", "nNA1", "nNA2")], function(y) ifelse(!is.na(y), formatC(y, format = "f", digits = digits), NA), FUN.VALUE = character(nrow(x$result)))
    print.object[, names(print.object) %in% c("n1", "n2", "nNA1", "nNA2")] <- vapply(print.object[,  names(print.object) %in% c("n1", "n2", "nNA1", "nNA2")], function(y) formatC(y, format = "f", digits = 0L), FUN.VALUE = character(nrow(x$result)))

    print.object <- rbind(c("Variable", "n1", "nNA1", "M1", "SD1",  "n2", "nNA2", "M2", "SD2", "M.Diff", "SD", "Estimate", "SE", "Low", "Upp"),
                            print.object)

    print.object[, 1L] <- format(print.object[, 1L], justify = "left")

    # Format
    print.object[, -1L] <- vapply(print.object[, -1L], function(y) format(y, justify = "right"), FUN.VALUE = character(nrow(x$result) + 1L))

    print.object <- vapply(print.object, formatC, format = "f", FUN.VALUE = character(nrow(x$result) + 1L))

    print.object[, 1L] <- paste(" ", print.object[, 1L])

  #---------------------------------------
  # Within-subject design
  } else {

    print.object[, !names(print.object) %in% c("var", "n", "nNA")] <- vapply(print.object[,  !names(print.object) %in% c("var", "n", "nNA")], function(y) formatC(y, format = "f", digits = digits), FUN.VALUE = character(1L))
    print.object[, names(print.object) %in% c("n", "nNA")] <- vapply(print.object[,  names(print.object) %in% c("n", "nNA")], function(y) formatC(y, format = "f", digits = 0L), FUN.VALUE = character(1L))

    print.object <- rbind(c("n", "nNA", "Variable1", "M1", "SD1", "Variable2", "M2", "SD2", "M.Diff", "SD", "Estimate", "SE", "Low", "Upp"),
                          print.object)

    print.object[, 1L] <- format(print.object[, 1L], justify = "left")

    # Format
    print.object[, -1L] <- vapply(print.object[, -1L], function(y) format(y, justify = "right"), FUN.VALUE = character(2))

    print.object <- vapply(print.object, formatC, format = "f", FUN.VALUE = character(2L))

    print.object[, 1L] <- paste(" ", print.object[, 1L])

  }

  ####################################################################################
  # Output

  #---------------------------------------
  # Between-subject design
  if(!isTRUE(x$args$paired)) {

    # Cohens d
    cat(paste0(" Cohen's d for bewteen-subject design with ", round(x$args$conf.level * 100L), "% confidence interval\n\n"))

  } else {

    # Cohens d
    cat(paste0(" Cohen's d for within-subject design with ", round(x$args$conf.level * 100L), "% confidence interval\n\n"))

  }

  # Output
  write.table(print.object, row.names = FALSE, col.names = FALSE, quote = FALSE)

  #---------------------------------------
  # Within-subject design
  if (!isTRUE(x$args$paired)) {

    if (is.null(x$args$ref)) {

      if (isTRUE(x$args$weighted)) {

        if (isTRUE(x$args$correct)) {

          cat("\n Note. SD = weighted pooled standard deviation \n       Applying small sample correction factor")

        } else {

          cat("\n Note. SD = weighted pooled standard deviation\n")

        }

      } else {

        cat("\n Note. SD = unweighted pooled standard deviation\n")

      }

    } else {

      cat(paste0("\n Note. SD = standard deviation of the reference group: ", x$args$ref, "\n"))

    }


  } else {

    if (is.null(x$args$ref)) {

      if (isTRUE(x$args$weighted)) {

        if (isTRUE(x$args$correct)) {

          cat("\n Note. SD = controlling for the correlation between measures \n       Applying small sample correction factor")

        } else {

          cat("\n Note. SD = controlling for the correlation between measures\n")

        }

      } else {

        cat("\n Note. SD = ignoring the correlation between measures\n")

      }

    } else {

      # Between-subject design
      if (!isTRUE(x$args$paired)) {

        cat(paste0("\n Note. SD = standard deviation of the reference group: ", x$args$ref, "\n"))

      # Within-subject design
      } else {

        cat(paste0("\n Note. SD = standard deviation of the reference variable: ", x$args$ref, "\n"))

      }

    }

  }

}
