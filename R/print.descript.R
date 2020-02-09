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
#' @param mat       logical: if \code{TRUE}, output by a grouping variable is shown in a matrix.
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
#'                   x3 = c(7, 8, 5, 6, 4, NA, 8, NA, 6, 5))
#'
#' # Descriptive statistics for x1
#' dat.descript <- descript(dat$x1, output = FALSE)
#'
#' # Print descript object with 3 digits
#' print(dat.descript, digits = 3)
print.descript <- function(x, print = x$args$print, mat = x$args$mat, sort.var = x$args$sort.var,
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
    # Check input 'mat'
    if (isFALSE(isTRUE(mat) | isFALSE(mat))) {

      stop("Please specify TRUE or FALSE for the argument 'mat'.", call. = FALSE)

    }

    #......
    # Check input 'sort.var'
    if (isFALSE(isTRUE(sort.var) | isFALSE(sort.var))) {

      stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  if (length(print) == 1 && print == "all") {

    print <- c("n", "nNA", "pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

  }

  ####################################################################################
  # Main Function

  # Variables to round
  print.round <- c("pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

  #----------------------------------------
  # No grouping
  if (is.null(x$args$group)) {

    #......
    # Print object
    print.object <- x$result

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
    print.object[, 1] <- format(print.object[, 1, drop = FALSE], justify = "left")

    print.object[, -1] <- apply(print.object[, -1, drop = FALSE], 2, function(y) format(y, justify = "right"))

    if (is.null(dim(x$data))) {

      print.object <- print.object[, -1]

    }

    print.object[, 1] <- paste0(" ", print.object[, 1])

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

  #----------------------------------------
  # Grouping
  } else {

    #......
    # Print object
    object.group <- x$result

    #......
    # Output as matrix
    if (isTRUE(mat)) {

      # Results as matrix
      object.group.matrix <- eval(parse(text = paste0("rbind(", paste0("object.group[[", 1:length(object.group), "]]", collapse = ", "), ")")))

      # Print object
      print.object <- data.frame(group = rep(names(object.group), each = unique(sapply(x$result, nrow))),
                                 variable = object.group.matrix$variable, object.group.matrix[, -1],
                                 stringsAsFactors = FALSE, check.names = FALSE)

      # Sort by variables
      if (isTRUE(sort.var)) {

        print.object <- print.object[order(print.object[, "variable"]), ]

      }

      # Round
      print.object[, print.round] <- sapply(print.round, function(y) formatC(print.object[, y], digits = digits, format = "f"))

      # Percentages
      print.object[, "pNA"] <- paste0(print.object[, "pNA"], "%")

      # Col names
      print.object <- rbind(c("Group", "Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object)

      # Select statistical measures and add variable names
      print.object <- data.frame(print.object[, c("group", "variable")], print.object[, -c(1, 2)][, print, drop = FALSE], stringsAsFactors = FALSE)

      # Format
      print.object[, 1] <- format(print.object[, 1], justify = "left")
      print.object[, 2] <- format(print.object[, 2], justify = "left")

      print.object[, -c(1:2)] <- apply(print.object[, -c(1:2)], 2, format, justify = "right")

      if (is.null(dim(x$data))) {

        print.object <- print.object[, -2]

      }

      print.object[1, 1] <- paste0(" ", print.object[1, 1], " ", collapse = "")
      print.object[-1, 1] <- paste0("  ", print.object[-1, 1])

      print.object[, -c(1:2)] <- apply(print.object[, -c(1:2)], 2, format, justify = "right")

      # Print Output
      write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    #......
    # Split output
    } else {

      # Sort by variable
      if (isTRUE(sort.var)) {

        object.group.matrix1 <- eval(parse(text = paste0("rbind(", paste0("object.group[[", 1:length(object.group), "]]", collapse = ", "), ")")))

        object.group.matrix2 <- data.frame(group = rep(names(object.group), each = length(unique(object.group.matrix1$variable))),
                                           variable = object.group.matrix1$variable, object.group.matrix1[, -1],
                                           stringsAsFactors = FALSE)

        # Print object
        print.object <- split(object.group.matrix2, f = object.group.matrix2$variable)

        # Format
        for (i in names(print.object)) {

          # Round
          print.object[[i]][, print.round] <- sapply(print.round, function(y) formatC(print.object[[i]][, y], digits = digits, format = "f"))

          # Percentages
          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          # Col names
          print.object[[i]] <- rbind(c("Group", "Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object[[i]])

          # Select statistical measures and add variable names
          print.object[[i]] <- data.frame(print.object[[i]][, c("group", "variable")], print.object[[i]][, print, drop = FALSE], stringsAsFactors = FALSE)

          # Format
          print.object[[i]][, 1] <- format(print.object[[i]][, 1], justify = "left")
          print.object[[i]][, 2] <- format(print.object[[i]][, 2], justify = "left")

          print.object[[i]][1, 1] <- paste0(" ", print.object[[i]][1, 1], " ", collapse = "")
          print.object[[i]][-1, 1] <- paste0("  ", print.object[[i]][-1, 1])

          print.object[[i]][, -c(1:2)] <- apply(print.object[[i]][, -c(1:2), drop = FALSE], 2, function(y) format(y, justify = "right"))

          if (is.null(dim(x$data))) {

            print.object[[i]] <- print.object[[i]][, -2]

          }

          print.object[[i]][, 1] <- paste0("  ", print.object[[i]][, 1])

        }

        # Print object
        for (i in names(print.object)) {

          if (is.null(dim(x$dat))) { cat(" Variable:", i, "\n") }

          write.table(print.object[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

          if (i != names(print.object)[length(print.object)]) { cat("\n") }

        }

      } else {

        # Print object
        print.object <- object.group

        # Format
        for (i in names(print.object)) {

          # Round
          print.object[[i]][, print.round] <- sapply(print.round, function(y) formatC(print.object[[i]][, y], digits = digits, format = "f"))

          # Percentages
          print.object[[i]][, "pNA"] <- paste0(print.object[[i]][, "pNA"], "%")

          # Col names
          print.object[[i]] <- rbind(c("Variable", "n", "nNA", "pNA", "M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt"), print.object[[i]])

          # Select statistical measures and add variable names
          print.object[[i]] <- data.frame(variable = print.object[[i]][, "variable"], print.object[[i]][, print, drop = FALSE], stringsAsFactors = FALSE)

          # Format
          print.object[[i]][, 1] <- format(print.object[[i]][, 1, drop = FALSE], justify = "left")

          print.object[[i]][, -1] <- apply(print.object[[i]][, -1, drop = FALSE], 2, function(y) format(y, justify = "right"))

          if (is.null(dim(x$data))) {

            print.object[[i]] <- print.object[[i]][, -1]

          }

          print.object[[i]][, 1] <- paste0("  ", print.object[[i]][, 1])

        }

        # Print object
        for (i in names(print.object)) {

          cat(" Group:", i, "\n")

          write.table(print.object[[i]], quote = FALSE, row.names = FALSE, col.names = FALSE)

          if (i != names(print.object)[length(print.object)]) { cat("\n") }

        }

      }

    }

  }

}
