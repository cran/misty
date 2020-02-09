#' Print freq object
#'
#' This function prints the \code{freq} object
#'
#' @param x           \code{freq} object.
#' @param print       a character string indicating which percentage(s) to be printed on the console, i.e.,
#'                    no percentages (\code{"no"}), all percentages (\code{"all"}), percentage frequencies
#'                    (\code{"print"}), and valid percentage frequencies (\code{"v.perc"}).
#' @param freq        logical: if \code{TRUE}, absolute frequencies will be shown on the console.
#' @param digits      an integer value indicating the number of decimal places to be used for displaying
#'                    percentages.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{freq}}
#'
#' @method print freq
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(x1 = c(3, 3, 2, 3, 2, 3, 3, 2, 1, -99),
#'                   x2 = c(2, 2, 1, 3, 1, 1, 3, 3, 2, 2),
#'                   y1 = c(1, 4, NA, 5, 2, 4, 3, 5, NA, 1),
#'                   y2 = c(2, 3, 4, 3, NA, 4, 2, 3, 4, 5),
#'                   z = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#'
#' # Frequency table for one variable
#' # convert value -99 into NA
#' dat.freq <- freq(dat$x1, as.na = -99, output = FALSE)
#'
#' # Print freq object with 3 digits for displaying percentages
#' print(dat.freq, digits = 3)
#' }
print.freq <- function(x, print = x$args$print, freq = x$args$freq, digits = x$args$digits,
                       check = TRUE, ...) {

  ####################################################################################
  # Input check

  if (isTRUE(check)) {

    #..................
    # Check input 'print'
    if (length(print) != 1) {

      stop("Please specify one of the character strings \"no\", \"all\", \"perc\", or \"v.perc\" for the argument 'print'.",
           call. = FALSE)

    }

    #..................
    # Check input 'print'
    if (!print %in% c("no", "all", "perc", "v.perc")) {

      stop("Character string in the argument 'print' does not match with \"no\", \"all\", \"perc\", or \"v.perc\".",
           call. = FALSE)

    }

    #..................
    # Check input 'freq'
    if (isFALSE(isTRUE(freq) || isFALSE(freq))) {

      stop("Please specify TRUE or FALSE for the argument 'freq'.", call. = FALSE)

    }

    #..................
    # No frequencies and percentages
    if (print == "no" && isFALSE(freq)) {

      # More than one variable and freq = FALSE
      if (!is.null(dim(x))) {

        print <- "all"

      # One variable and freq = FALSE and print = "no"
      } else {

        stop("Please specify print = \"all\", print = \"print\", or print = \"v.perc\" when specifying freq = TRUE.", call. = FALSE)

      }

    }

    #..................
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Main function

  # Print object
  print.object <- x$result

  # Data with user-specified missing data
  if (!is.null(x$args$as.na)) {

    x$data <- misty::as.na(x$data, na = x$args$as.na, check = check)

  }

  #-----------------------------------------
  # One variable

  if (ncol(as.data.frame(x$data)) == 1 || (isTRUE(x$args$split) && ncol(x$data) == 1)) {

    #..................
    # Values in rows
    if (isFALSE(x$args$val.col)) {

      print.object <- data.frame(x = c("Value", rep("", nrow(print.object) - 1), "Missing", "Total"),
                                 val = c(print.object[1:(grep("NA", print.object$Value) - 1), 1], "Total", "NA", ""),
                                 rbind(print.object[1:(grep("NA", print.object$Value) - 1), -1],
                                       apply(print.object[1:(grep("NA", print.object$Value) - 1), -1], 2, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                       print.object[grep("NA", print.object$Value), -1],
                                       c(sum(as.numeric(print.object$Freq), na.rm = TRUE), "100", "")),
                                 stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

      # Round digits
      print.object[, c("Perc", "V.Perc")] <- suppressWarnings(apply(print.object[, c("Perc", "V.Perc")], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f"), "%")))

      # Remove NA
      print.object[, "V.Perc"] <- gsub("NA%", "  ", print.object[, "V.Perc"])

      # Format
      print.object[, 1:2] <- apply(print.object[, 1:2], 2, function(y) format(y, justify = "left"))

      print.object[, -c(1:2)] <- apply(print.object[, -(1:2)], 2, function(y) format(y, justify = "right"))

      #......
      # Omit Total row if there are no missing values
      if (all(!is.na(x$data))) {

        # Object without Total column
        print.object <- print.object[-grep("Total", print.object$x),  ]

        # Object without valid percentage
        print.object <- print.object[, -grep("V.Perc", colnames(print.object))]

      }

      #......
      # Omit Missing and Total row if print = "v.valid" and freq = FALSE
      if (length(print) == 1 && print == "v.perc" && isFALSE(freq)) {

        # Object without Total row
        print.object <- print.object[-grep("Total", print.object$x),  ]

        # Object without Missing row
        print.object <- print.object[-grep("Missing", print.object$x),  ]

      }

      #......
      # Omit absolute frequencies if freq = FALSE
      if (!isTRUE(freq)) {

        print.object <- print.object[, -grep("Freq", colnames(print.object))]

      }

      #......
      # Omit percentages

      ###
      # Omit percentages if !"perc" %in% print
      if (!"perc" %in% print || "no" %in% print) {

        # Object without percentage
        print.object <- print.object[, -which(colnames(print.object) == "Perc")]

      }

      ###
      # Omit valid percentages if !"v.perc" %in% print
      if ("V.Perc" %in% colnames(print.object)) {

        if (!"v.perc" %in% print || "no" %in% print) {

          # Object without valid percentage
          print.object <- print.object[, -which(colnames(print.object) == "V.Perc")]

        }

      }

      # Column names
      colnames(print.object)[1:2] <- c("", "")

    #..................
    # Values in columns
    } else {

      print.object <- data.frame(print.object[, -ncol(print.object)],
                                 val = apply(print.object[, -ncol(print.object)], 1, sum, na.rm = TRUE),
                                 nNA = print.object[, ncol(print.object)],
                                 total = c(sum(print.object[1, ], na.rm = TRUE), "100", ""),
                                 stringsAsFactors = FALSE, check.names = FALSE)

      print.object[1, ] <- as.character(print.object[1, ])
      print.object[2, ] <- paste0(formatC(as.numeric(print.object[2, ]), digits = digits, format = "f"), "%")
      print.object[3, ] <- paste0(formatC(as.numeric(print.object[3, ]), digits = digits, format = "f"), "%")

      print.object[3, ] <- gsub("NA%", "", print.object[3,  ])

      # Row names
      print.object <- cbind(x = c("Freq", "Perc", "V.Perc"), print.object)

      # Column names
      colnames(print.object) <- c("Value", colnames(x$result)[-length(x$result)], "Total", "Missing", "Total")

      # Format
      print.object[, 1] <- format(print.object[, 1], justify = "left")
      colnames(print.object)[1] <- format(c(colnames(print.object)[1], print.object[, 1]), justify = "left")[1]

      print.object[, -1] <- apply(print.object[, -1], 2, function(y) format(y, justify = "right"))

      #......
      # Omit Total and V.Perc column if there are no missing values
      if (all(!is.na(x$data))) {

        # Object without Total column
        print.object <- print.object[, -ncol(print.object)]

        # Object without valid percentage
        print.object <- print.object[-grep("V.Perc", print.object[, 1]), ]

      }

      #......
      # Omit Missing and Total row if perc = "v.valid" and freq = FALSE
      if (length(print) == 1 && print == "v.perc" && isFALSE(freq)) {

        # Object without Total column
        print.object <- print.object[, -ncol(print.object)]

        # Object without Missing column
        print.object <- print.object[, -ncol(print.object)]

      }

      #......
      # Omit absolute frequencies if freq = FALSE
      if (!isTRUE(freq)) {

        print.object <- print.object[-grep("Freq", print.object[, 1]), ]

      }

      #......
      # Omit percentages

      ###
      # Omit percentages if !"perc" %in% print
      if (!"perc" %in% print || "no" %in% print) {

        # Object without percentage
        print.object <- print.object[-which(row.names(print.object) == "Perc"), ]

      }

      ###
      # Omit valid percentages if !"v.perc" %in% print
      if ("V.Perc" %in% row.names(print.object)) {

        if (!"v.perc" %in% print || "no" %in% print) {

          # Object without valid percentage
          print.object <- print.object[-which(row.names(print.object) == "V.Perc"), ]

        }

      }

    }

    ####################################################################################
    # Output

    print(print.object, row.names = FALSE, max = 99999)

  }

  #-----------------------------------------
  # More than one variable

  if (ncol(as.data.frame(x$data)) > 1) {

    #........................................
    # split = FALSE
    if (isFALSE(x$args$split)) {

      #..................
      # Values in rows
      if (isFALSE(x$args$val.col)) {

        #....
        # Absolute frequencies
        if (isTRUE(freq)) {

          print.object$freq <- data.frame(x = c("Value", rep("", nrow(print.object$freq) - 1), "Missing", "Total"),
                                          val = c(print.object$freq[1:(nrow(print.object$freq) - 1), 1], "Total", "NA", ""),
                                          rbind(print.object$freq[1:(nrow(print.object$freq) - 1), -1],
                                                apply(print.object$freq[1:(nrow(print.object$freq) - 1), -1], 2, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                                print.object$freq[nrow(print.object$freq), -1],
                                                apply(print.object$freq[, -1], 2, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                          stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

          # Format
          print.object$freq[, 1:2] <- apply(print.object$freq[, 1:2], 2, function(y) format(y, justify = "left"))

          print.object$freq[, -c(1:2)] <- apply(print.object$freq[, -(1:2)], 2, function(y) format(y, justify = "right"))

          # No missing data
          if (all(!is.na(x$data))) {

            print.object$freq <- print.object$freq[-grep("Total",  print.object$freq$x), ]
            print.object$freq$val <- format(misty::trim(print.object$freq$val), justify = "left")

          }

          # Column names
          colnames(print.object$freq)[1:2] <- c("", "")

        }

        #....
        # Percentages
        if (all(print != "no") && "perc" %in% print) {

          print.object$perc <- data.frame(x = c("Value", rep("", nrow(print.object$perc) - 1), "Missing", "Total"),
                                          val = c(print.object$perc[1:(nrow(print.object$perc) - 1), 1], "Total", "NA", ""),
                                          rbind(print.object$perc[1:(nrow(print.object$perc) - 1), -1],
                                                apply(print.object$perc[1:(nrow(print.object$perc) - 1), -1], 2, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                                print.object$perc[nrow(print.object$perc), -1],
                                                apply(print.object$perc[, -1], 2, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                          stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

          # Round digits
          print.object$perc[, -c(1:2)] <- apply(print.object$perc[, -c(1:2)], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f"), "%"))

          # Format
          print.object$perc[, 1:2] <- apply(print.object$perc[, 1:2], 2, function(y) format(y, justify = "left"))

          print.object$perc[, -c(1:2)] <- apply(print.object$perc[, -(1:2)], 2, function(y) format(y, justify = "right"))

          # No missing data
          if (all(!is.na(x$data))) {

            print.object$perc <- print.object$perc[-grep("Total",  print.object$perc$x), ]
            print.object$perc$val <- format(misty::trim(print.object$perc$val), justify = "left")

          }

          # Column names
          colnames(print.object$perc)[1:2] <- c("", "")

        }

        #....
        # Valid percentages
        if (all(print != "no") && "v.perc" %in% print) {

          print.object$v.perc <- data.frame(x = c("Value", rep("", nrow(print.object$v.perc) - 1), "Total"),
                                            val = c(print.object$v.perc[, 1], ""),
                                            rbind(print.object$v.perc[, -1],
                                                  apply(print.object$v.perc[, -1], 2, function(y) sum(as.numeric(y), na.rm = TRUE))),
                                            stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

          # Round digits
          print.object$v.perc[, -c(1:2)] <- apply(print.object$v.perc[, -c(1:2)], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f"), "%"))

          # Format
          print.object$v.perc[, 1:2] <- apply(print.object$v.perc[, 1:2], 2, function(y) format(y, justify = "left"))

          # Format
          print.object$v.perc[, -c(1:2)] <- apply(print.object$v.perc[, -(1:2)], 2, function(y) format(y, justify = "right"))

          # Column names
          colnames(print.object$v.perc)[1:2] <- c("", "")

        }

      #..................
      # Values in columns
      } else {

        #....
        # Absolute frequencies
        if (isTRUE(freq)) {

          print.object$freq <- data.frame(print.object$freq[, 1:(ncol(print.object$freq) - 1)],
                                          val = apply(print.object$freq[, 2:(ncol(print.object$freq) - 1)], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                          miss = print.object$freq[, ncol(print.object$freq)],
                                          total = apply(print.object$freq[, 2:(ncol(print.object$freq))], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                          check.names = FALSE, stringsAsFactors = FALSE)

          # Add variable names
          colnames(print.object$freq) <- c("", colnames(print.object$freq)[2:(ncol(print.object$freq) - 3)], "Total", "Missing", "Total")

          # Format
          print.object$freq[, 1] <- format(print.object$freq[, 1], justify = "left")

          print.object$freq[, -1] <- apply(print.object$freq[, -1], 2, function(y) format(y, justify = "right"))

          # No missing data
          if (all(!is.na(x$data))) {

            print.object$freq <- print.object$freq[, -ncol(print.object$freq)]

          }

        }

        #....
        # Percentages
        if (all(print != "no") && "perc" %in% print) {

          print.object$perc <- data.frame(print.object$perc[, 1:(ncol(print.object$perc) - 1)],
                                          val = apply(print.object$perc[, 2:(ncol(print.object$perc) - 1)], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                          miss = print.object$perc[, ncol(print.object$perc)],
                                          total = apply(print.object$perc[, 2:(ncol(print.object$perc))], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                          check.names = FALSE, stringsAsFactors = FALSE)

          # Add variable names
          colnames(print.object$perc) <- c("", colnames(print.object$perc)[2:(ncol(print.object$perc) - 3)], "Total", "Missing", "Total")

          # Round digits
          print.object$perc[, -1] <- apply(print.object$perc[, -1], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f"), "%"))

          # Format
          print.object$perc[, 1] <- format(print.object$perc[, 1], justify = "left")

          print.object$perc[, -1] <- apply(print.object$perc[, -1], 2, function(y) format(y, justify = "right"))

          # No missing data
          if (all(!is.na(x$data))) {

            print.object$perc <- print.object$perc[, -ncol(print.object$perc)]

          }

        }

        #....
        # Valid percentages
        if (all(print != "no") && "v.perc" %in% print) {

          print.object$v.perc <- data.frame(print.object$v.perc,
                                            total = apply(print.object$v.perc[, 2:(ncol(print.object$v.perc))], 1, function(y) sum(as.numeric(y), na.rm = TRUE)),
                                            check.names = FALSE, stringsAsFactors = FALSE)

          # Add variable names
          colnames(print.object$v.perc) <- c("", colnames(print.object$v.perc)[2:(ncol(print.object$v.perc) - 1)], "Total")

          # Round digits
          print.object$v.perc[, -1] <- apply(print.object$v.perc[, -1], 2, function(y) paste0(formatC(as.numeric(y), digits = digits, format = "f"), "%"))

          # Format
          print.object$v.perc[, 1] <- format(print.object$v.perc[, 1], justify = "left")

          print.object$v.perc[, -1] <- apply(print.object$v.perc[, -1], 2, function(y) format(y, justify = "right"))

        }

      }

      ####################################################################################
      # Output

      #..................
      # Absolute frequencies
      if (isTRUE(freq)) {

        cat("Frequencies\n")
        print(print.object$freq, row.names = FALSE, max = 99999)

      }

      #..................
      # Percentage frequencies
      if (all(print != "no")) {

        if (isTRUE(freq)) { cat("\n") }

        # Percentages
        if ("perc" %in% print) {

          cat("Percentages\n")
          print(print.object$perc, row.names = FALSE, max = 99999)

        }

        if (any(is.na(x$data))) {

          # Valid percentages
          if ("v.perc" %in% print) {

            if ("perc" %in% print) { cat("\n") }

            cat("Valid Percentages\n")
            print(print.object$v.perc, quote = FALSE, row.names = FALSE, max = 99999)

          }

        }

      }

    # split = TRUE
    } else {

      for (i in names(x$result)) {

        cat("\n", paste0("$", i), "\n", sep = "")

        temp <- list(call = x$call, data = x$data[, i], args = x$args, result = x$result[[i]])
        class(temp) <- "freq"

        print(temp, check = FALSE)

      }

    }

  }

}
