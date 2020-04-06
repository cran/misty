#' Print descript object
#'
#' This function prints the \code{descript} object
#'
#' @param x         \code{descript} object.
#' @param print     a character vector indicating which statistical measures to be printed on the console,
#'                  i.e. \code{n} (number of observations), \code{nNA} (number of missing values),
#'                  \code{pNA} (percentage of missing values), \code{m} (arithmetic mean), \code{var} (variance),
#'                  \code{sd} (standard deviation), \code{med} (median),\code{min} (minimum),
#'                  \code{p25} (25th percentile, first quartile), \code{p75} (75th percentile, third quartile),
#'                  \code{max} (maximum),  \code{range} (range), \code{iqr} (interquartile range),
#'                  \code{skew} (skewness), and \code{kurt} (excess kurtosis).
#' @param sort.var  logical: if \code{TRUE}, output is sorted by variables.
#' @param digits    an integer value indicating the number of decimal places to be used.
#' @param check     logical: if \code{TRUE}, argument specification is checked.
#' @param ...       further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{descript}}
#'
#' @method print descript
#'
#' @export
#'
#' @examples
#' dat <- data.frame(group = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2),
#'                   x1 = c(3, 1, 4, 2, 5, 3, 2, 4, NA, 4),
#'                   x2 = c(4, NA, 3, 6, 3, 7, 2, 7, 5, 1),
#'                   x3 = c(7, 8, 5, 6, 4, NA, 8, NA, 6, 5), stringsAsFactors = FALSE)
#'
#' # Descriptive statistics for x1
#' dat.descript <- descript(dat$x1, output = FALSE)
#'
#' # Print descript object with 3 digits
#' print(dat.descript, digits = 3)
print.descript <- function(x, print = x$args$print, sort.var = x$args$sort.var,
                           digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input 'print'
    if (!all(print %in%  c("all", "n", "nNA", "pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "skew",  "range", "iqr", "kurt"))) {

      stop("Character strings in the argument 'print' do not all match with \"all\", \"n\", \"nNA\", \"pNA\", \"m\", \"var\", \"sd\", \"min\", \"p25\", \"med\", \"p75\", \"max\", \"range\", \"iqr\", \"skew\", or \"kurt\".",
           call. = FALSE)

    }

    #......
    # Check input 'sort.var'
    if (isFALSE(isTRUE(sort.var) || isFALSE(sort.var))) {

      stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  if (length(print) == 1L && print == "all") {

    print <- c("n", "nNA", "pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

  }

  ####################################################################################
  # Main Function

  #......
  # Variables to round
  print.round <- c("pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

  #......
  # Print object
  print.object <- x$result

  #----------------------------------------
  # No grouping

  if (is.null(x$data$group) && is.null(x$data$split)) {

    #......
    # Round
    print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]), formatC(print.object[, y], digits = digits, format = "f"), NA))

    #......
    # Percentages
    print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

    #......
    # Row names
    print.object <- rbind(c("Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object)

    #......
    # Select statistical measures and add variable names
    print.object <- data.frame(var = print.object[, "variable"], print.object[, print, drop = FALSE], stringsAsFactors = FALSE,
                               check.names = FALSE)

    #......
    # Format
    print.object[, 1L] <- format(print.object[, 1L, drop = FALSE], justify = "left")

    print.object[, -1L] <- apply(print.object[, -1L, drop = FALSE], 2, function(y) format(y, justify = "right"))

    if (ncol(x$data$x) == 1L) {

      print.object <- print.object[, -1L]

    }

    print.object[, 1L] <- paste0(" ", print.object[, 1L])

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  }

  #----------------------------------------
  # Grouping

  if (!is.null(x$data$group) && is.null(x$data$split)) {

    # Sort by variables
    if (isTRUE(sort.var)) {

      print.object <- print.object[order(print.object[, "variable"]), ]

    }

    # Round
    print.object[, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[, y]),
                                                                          formatC(print.object[, y], digits = digits, format = "f"), NA))

    # Percentages
    print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

    # Col names
    print.object <- rbind(c("Group", "Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"),
                          print.object)

    # Select statistical measures and add variable names
    print.object <- data.frame(print.object[, c("group", "variable")], print.object[, -c(1, 2)][, print, drop = FALSE], stringsAsFactors = FALSE)

    # Format
    print.object[, 1L] <- format(print.object[, 1L], justify = "left")
    print.object[, 2L] <- format(print.object[, 2L], justify = "left")

    print.object[, -c(1L:2L)] <- apply(print.object[, -c(1L:2L)], 2, format, justify = "right")

    if (ncol(x$data$x) == 1L) {

      print.object <- print.object[, -2L]

    }

    print.object[1L, 1L] <- paste0(" ", print.object[1L, 1L], " ", collapse = "")
    print.object[-1L, 1L] <- paste0("  ", print.object[-1L, 1L])

    print.object[, -c(1L:2L)] <- apply(print.object[, -c(1L:2L)], 2, format, justify = "right")

    # Print Output
    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  #----------------------------------------
  # Split
  } else if (!is.null(x$data$split)) {

    # Format
    for (i in names(print.object)) {

      # Round
      print.object[[i]][, print.round] <- sapply(print.round, function(y) ifelse(!is.na(print.object[[i]][, y]), formatC(print.object[[i]][, y], digits = digits, format = "f"), NA))

      # Percentages
      print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

      #......
      # No grouping
      if (is.null(x$data$group)) {

        # Col names
        print.object[[i]] <- rbind(c("Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object[[i]])

        # Select statistical measures and add variable names
        print.object[[i]] <- data.frame(variable = print.object[[i]][, "variable"], print.object[[i]][, print, drop = FALSE], stringsAsFactors = FALSE)

        if (ncol(x$data$x) == 1L) {

          print.object[[i]] <- print.object[[i]][, -1L]

        }

      #......
      # Grouping
      } else {

        # Sort by variables
        if (isTRUE(sort.var)) {

          print.object[[i]] <- print.object[[i]][order(print.object[[i]][, "variable"]), ]

        }

        # Col names
        print.object[[i]] <- rbind(c("Group", "Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object[[i]])

        # Select statistical measures and add variable names
        print.object[[i]] <- data.frame(variable = print.object[[i]][, c("group", "variable")], print.object[[i]][, print, drop = FALSE], stringsAsFactors = FALSE)

        if (ncol(x$data$x) == 1L) {

          print.object[[i]] <- print.object[[i]][, -2L]

        }

      }

      # Format
      if (ncol(x$data$x) == 1L && is.null(x$data$group)) {

        print.object[[i]][-1L, 1L] <- paste0("  ", print.object[[i]][-1L, 1L])
        print.object[[i]] <- format(print.object[[i]], justify = "right")

      } else {

        print.object[[i]][, 1L] <- format(print.object[[i]][, 1L], justify = "left")
        print.object[[i]][, 2L] <- format(print.object[[i]][, 2L], justify = "right")

        print.object[[i]][1L, 1L] <- paste0(" ", print.object[[i]][1L, 1L], " ", collapse = "")
        print.object[[i]][-1L, 1L] <- paste0("  ", print.object[[i]][-1L, 1L])

        print.object[[i]][, -c(1L:2L)] <- apply(print.object[[i]][, -c(1L:2L), drop = FALSE], 2, function(y) format(y, justify = "right"))

        print.object[[i]][, 1L] <- paste0("  ", print.object[[i]][, 1L])

      }

    }

    # Print object
    for (i in names(print.object)) {

      cat(" Split Group:", i, "\n")

      write.table(print.object[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

      if (i != names(print.object)[length(print.object)]) { cat("\n") }

    }

  }

}
