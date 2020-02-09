#' Print crosstab
#'
#' This function prints the \code{crosstab} object
#'
#' @param x           \code{crosstab} object.
#' @param print       a character string or character vector indicating which percentage(s) to be printed on the
#'                    console, i.e., no percentages (\code{"no"}) (default), all percentages (\code{"all"}),
#'                    row-wise percentages (\code{"row"}), column-wise percentages (\code{"col"}), and
#'                    total percentages (\code{"total"}).
#' @param freq        logical: if \code{TRUE}, absolute frequencies will be included in the cross tabulation.
#' @param split       logical: if \code{TRUE}, output table is split in absolute frequencies and percentage(s).
#' @param digits      an integer indicating the number of decimal places digits to be used for displaying percentages.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param ...         further arguments passed to or from other methods.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{crosstab}}
#'
#' @method print crosstab
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- data.frame(x1 = c(1, 2, 2, 1, 1, 2, 2, 1, 1, 2),
#'                   x2 = c(NA, 2, 2, 1, 2, 1, 1, 1, 2, 1),
#'                   x3 = c(1, 2, 1, 1, 1, 2, 2, 2, 2, 1))
#'
#' # Cross Tabulation for x1 and x2
#' dat.crosstab <- crosstab(dat[, c("x1", "x2")], output = FALSE)
#'
#' # Print crosstab object with 3 digits for displaying percentages
#' print(dat.crosstab, digits = 3)
#' }
print.crosstab <- function(x, print = x$args$print, freq = x$args$freq, split = x$args$split,
                           digits = x$args$digits, check = TRUE, ...) {

  ####################################################################################
  # Input Check

  if (isTRUE(check)) {

    #......
    # Check input print
    if (any(!print %in% c("no", "all", "row", "col", "total"))) {

      stop("Character string(s) in the argument 'print' does not match with \"no\", \"all\", \"row\", \"col\" or \"total\".",
            call. = FALSE)

    }

    #......
    # Check input 'freq'
    if (isFALSE(isTRUE(freq) | isFALSE(freq))) {

      stop("Please specify TRUE or FALSE for the argument 'freq'.", call. = FALSE)

    }

    #......
    # Check print = "no" and freq = FALSE
    if (print == "no" && isFALSE(freq)) {

      stop("Please include either percentages (i.e., print != 'no') or absolute frequencies (i.e., freq = TRUE) in the cross tabulation.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  #......
  # Percentages
  no.perc <- if (any("no" %in% print)) {

    no.perc <- NULL

  } else {

    no.perc <- c("row", "col", "total")[!c("row", "col", "total") %in% print]

  }

  #......
  # Print object
  print.object <- x$result

  #----------------------------------------
  # Two-Dimensional Matrix

  if (ncol(x$data) == 2) {

    restab <- cbind(rep(names(print.object$freq.a[, 1]), times = 4),
                    rep(c("Freq", "Row %", "Col %", "Tot %"), each = nrow(print.object$freq.a)),
                    rbind(print.object$freq.a, print.object$perc.r, print.object$perc.c, print.object$perc.t),
                    c(apply(print.object$freq.a, 1, sum), rep("", times = 3*nrow(print.object$freq.a))))

    #......
    # Sort table

    # First variable is a factor
    if (is.factor(x$data[, 1])) {

      # Sort with NA
      if (any(is.na(x$data)) && isFALSE(x$args$na.omit)) {

        restab <- restab[order(factor(restab[, 1], levels = c(levels(x$data[, 1]), "NA"), labels = c(levels(x$data[, 1]), "NA"))), ]

      # Sort without NA
      } else {

        restab <- restab[order(factor(restab[, 1], levels = levels(x$data[, 1]), labels = levels(x$data[, 1]))), ]

      }

    # First variable is not a factor
    } else {

      restab <- restab[order(restab[, 1]), ]

    }

    #......
    # Frequencies and percentages

    # No absolute frequencies
    if (isFALSE(freq)) {

      restab <- restab[-grep("Freq", restab[, 2]), ]

    }

    # No percentages
    if (any(print == "no")) {

      restab <- restab[-grep(" %", restab[, 2]), -2]

    } else {

      # No total percentages
      if ("total" %in% no.perc) {

        restab <- restab[-grep("Tot %", restab[, 2]), ]

      }

      # No row percentages
      if ("row" %in% no.perc) {

        restab <- restab[-grep("Row %", restab[, 2]), ]

      }

      # No col percentages
      if ("col" %in% no.perc) {

        restab <- restab[-grep("Col %", restab[, 2]), ]

      }

    }

    #......
    # Format

    # Frequencies only
    if (any(print == "no")) {

      #......
      # Variable names and column sum
      restab <- rbind(c("", colnames(x$data)[2], rep("", times = (ncol(restab) - 2))),
                      c(colnames(x$data)[1], colnames(restab)[-c(1, ncol(restab))], "Total"),
                      restab,
                      c("Total", apply(print.object$freq.a, 2, sum), sum(print.object$freq.a)))

      # Format
      restab[2, 1] <- paste0(" ", restab[2, 1])
      restab[-c(1, 2), 1] <- paste0("  ", restab[-c(1, 2), 1])

      # Justify right
      restab[-2, 1] <- format(restab[-2, 1], justify = "right")
      restab[, 1] <- format(restab[, 1])

      restab[-1, -1] <- apply(restab[-1, -1], 2, function(y) format(y, justify = "right"))

      restab[-1, 2] <- paste0(" ", restab[-1, 2])

    # Percentage(s)
    } else {

      #......
      # Variable names and column sum
      restab <- rbind(c("", "", colnames(x$data)[2], rep("", times = (ncol(restab) - 3))),
                      c(colnames(x$data)[1], colnames(restab)[-c(1, ncol(restab))], "Total"),
                      restab,
                      c("Total", "", apply(print.object$freq.a, 2, sum), sum(print.object$freq.a)))

      # Format percentages
      restab[grep("%", restab[, 2]), -c(1:2, ncol(restab))] <- apply(restab[grep("%", restab[, 2]), -c(1:2, ncol(restab))], 2, function(y) paste0(format(formatC(as.numeric(y), digits = digits, format = "f"), justify = "right"), "%"))

      restab <- gsub("NaN", "NA", restab)

      # Justify right and left
      restab[, 1] <- format(restab[, 1], justify = "right")

      restab[2, 1] <- paste0(" ", sub("^\\s+", "", restab[2, 1]), paste0(rep(" ", times = max(nchar(restab[, 1])) - nchar(colnames(x$data)[1])), collapse = ""), " ")
      restab[-2, 1] <- paste0("  ", restab[-2, 1])

      restab[, 2] <- format(restab[, 2], justify = "left")

      restab[-1, -c(1:2)] <- apply(restab[-1, -c(1:2)], 2, format, justify = "right")

      restab[-1, 3] <- paste0(" ", restab[-1, 3])

    }

    #......
    # Output table not split
    if (isFALSE(split) | all(print == "no")) {

      # Remove Total row and column
      if (isFALSE(freq)) {

        restab <- restab[-nrow(restab), -ncol(restab)]

      }

      # Remove duplicated row labels
      restab[, 1] <- ifelse(duplicated(restab[, 1]), paste(rep(" ", times = unique(nchar(restab[, 1]))), collapse = ""), restab[, 1])

      # Print results
      write.table(restab, col.names = FALSE, row.names = FALSE, quote = FALSE)

    # Output table split
    } else {

      if (isTRUE(freq)) {

        # Frequencies
        restab.abs <- restab[-grep("%", restab[, 2]), -2]

        restab.abs[-1, -1] <- apply(restab.abs[-1, -1], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.abs[-1, 2] <- paste0(" ", restab.abs[-1, 2])

        restab.abs[, 1] <- paste0(" ", restab.abs[, 1])

        cat(" Frequencies\n")
        write.table(restab.abs, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Row-wise percentages
      if ("row" %in% print) {

        if (isTRUE(freq)) { cat("\n") }

        restab.row <- cbind(rbind(restab[1:2, -c(2, ncol(restab))], restab[grep("Row", restab[, 2]), -c(2, ncol(restab))]),
                            c("", "Total",
                              rep(ifelse(digits == 0, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = nrow(restab[grep("Row", restab[, 2]), ]))))

        restab.row[which(apply(restab.row, 1, function(y) length(grep("NA%", y)) != 0)), ncol(restab.row)] <- "NA%"

        restab.row[, ncol(restab.row)] <- format(restab.row[, ncol(restab.row)], justify = "right")

        restab.row[-1, -1] <- apply(restab.row[-1, -1], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.row[-1, 2] <- paste0(" ", restab.row[-1, 2])

        restab.row[, 1] <- paste0(" ", restab.row[, 1])

        cat(" Row-Wise Percentages\n")
        write.table(restab.row, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Column-wise percentages
      if ("col" %in% print) {

        if (isTRUE(freq) | "row" %in% print) { cat("\n") }

        restab.col <- rbind(restab[1:2, -c(2, ncol(restab))], restab[grep("Col", restab[, 2]), -c(2, ncol(restab))],
                            c("Total", rep(ifelse(digits == 0, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = ncol(restab) - 3)))

        restab.col[nrow(restab.col), which(apply(restab.col, 2, function(y) length(grep("NA%", y)) != 0))] <- "NA%"

        restab.col[-1, ] <- apply(restab.col[-1, ], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.col[, 1] <- format(restab.col[, 1])

        restab.col[-1, 2] <- paste0(" ", restab.col[-1, 2])

        restab.col[, 1] <- paste0("  ", restab.col[, 1])

        cat(" Column-Wise Percentages\n")
        write.table(restab.col, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Total percentages
      if ("total" %in% print) {

        if (isTRUE(freq) | "row" %in% print | "col" %in% print) { cat("\n") }

        restab.total <- rbind(restab[1:2, -c(2, ncol(restab))], restab[grep("Tot", restab[, 2]), -c(2, ncol(restab))])

        restab.total[-1, -1] <- apply(restab.total[-1, -1], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.total <- rbind(cbind(restab.total, c("", "Total", rep("", times = nrow(restab.total) - 2))), c(rep("", times = ncol(restab.total)),
                                                                                                            ifelse(digits == 0, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%"))))

        restab.total[, ncol(restab.total)] <- format(restab.total[, ncol(restab.total)], justify = "right")

        restab.total[-1, ] <- apply(restab.total[-1, ], 2, format)

        restab.total[-1, 2] <- paste0(" ", restab.total[-1, 2])

        restab.total[, 1] <- paste0(" ", restab.total[, 1])

        cat(" Total Percentages\n")
        write.table(restab.total, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

    }

  }

  #----------------------------------------
  # Three-Dimensional Matrix

  if (ncol(x$data) == 3) {

    # Absolute frequencies
    freq.a.print <- NULL
    for (i in seq_len(nrow(print.object$freq.a[[1]]))) {

      freq.a.print <- cbind(freq.a.print,
                            unlist(lapply(print.object$freq.a, function(y) y[i, ])))

    }

    # Row %
    perc.r.print <- NULL
    for (i in seq_len(nrow(print.object$perc.c[[1]]))) {

      perc.r.print <- cbind(perc.r.print,
                            unlist(lapply(print.object$perc.c, function(y) y[i, ])))

    }

    # Column %
    perc.c.print <- NULL
    for (i in seq_len(nrow(print.object$perc.r[[1]]))) {

      perc.c.print <- cbind(perc.c.print,
                            unlist(lapply(print.object$perc.r, function(y) y[i, ])))

    }

    # Total %
    perc.t.print <- NULL
    for (i in seq_len(nrow(print.object$perc.t[[1]]))) {

      perc.t.print <- cbind(perc.t.print,
                            unlist(lapply(print.object$perc.t, function(y) y[i, ])))

    }

    # Result table
    restab <- cbind(rep(names(print.object$freq.a), each = ncol(print.object$freq.a[[1]]), time = 4),
                    rep(colnames(print.object$freq.a[[1]]), times = 4*length(print.object$freq.a)),
                    rep(c("Freq", "Row %", "Col %", "Tot %"), each =  ncol(print.object$freq.a[[1]])*length(print.object$freq.a)),
                    rbind(freq.a.print, perc.r.print, perc.c.print, perc.t.print),
                    c(apply(freq.a.print, 1, sum), rep("", times = 3*length(print.object$freq.a)*ncol(print.object$freq.a[[1]]))))

    # Convert NaN in NA
    restab <- gsub("NaN", NA, restab)

    #......
    # Sort table

    # First and scond variable are a factor
    if (is.factor(x$data[, 1]) && is.factor(x$data[, 2])) {

      # Sort with NA
      if (any(is.na(x$data)) && isFALSE(x$args$na.omit)) {

        restab <-restab[order(factor(restab[, 1], levels = c(levels(x$data[, 1]), "NA"), labels = c(levels(x$data[, 1]), "NA")),
                        factor(restab[, 2], levels = c(levels(x$data[, 2]), "NA"), labels = c(levels(x$data[, 2]), "NA"))), ]

      # Sort without NA
      } else {

        restab <- restab[order(factor(restab[, 1], levels = levels(x$data[, 1]), labels = levels(x$data[, 1])),
                               factor(restab[, 2], levels = levels(x$data[, 2]), labels = levels(x$data[, 2]))), ]

      }

    }

    # First variable is a factor, second variable is not a factor
    if (is.factor(x$data[, 1]) && !is.factor(x$data[, 2])) {

      # Sort with NA
      if (any(is.na(x$data)) && isFALSE(x$args$na.omit)) {

        restab <- restab[order(factor(restab[, 1], levels = c(levels(x$data[, 1]), "NA"), labels = c(levels(x$data[, 1]), "NA")),
                               restab[, 2]), ]

      # Sort without NA
      } else {

        restab <- restab[order(factor(restab[, 1], levels = levels(x$data[, 1]), labels = levels(x$data[, 1])),
                               restab[, 2]), ]

      }

    }

    # First variable is not a factor, second variable is a factor
    if (!is.factor(x$data[, 1]) && is.factor(x$data[, 2])) {

      # Sort with NA
      if (any(is.na(x$data)) && isFALSE(x$args$na.omit)) {

        restab <- restab[order(restab[, 1],
                               factor(restab[, 2], levels = c(levels(x$data[, 2]), "NA"), labels = c(levels(x$data[, 2]), "NA"))), ]

      # Sort without NA
      } else {

        restab <- restab[order(restab[, 1],
                               factor(restab[, 2], levels = levels(x$data[, 2]), labels = levels(x$data[, 2]))), ]

      }

    }

    # First and second variable are not a factor
    if (!is.factor(x$data[, 1]) && !is.factor(x$data[, 2])) {

      # Sort with NA
      if (any(is.na(x$data)) && isFALSE(x$args$na.omit)) {

        restab <- restab[order(restab[, 1],
                               factor(restab[, 2], levels = c(levels(x$data[, 2]), "NA"), labels = c(levels(x$data[, 2]), "NA"))), ]

        # Sort without NA
      } else {

        restab <- restab[order(restab[, 1], restab[, 2]), ]

      }

    }

    #......
    # Frequencies and percentages

    # No absolute frequencies
    if (isFALSE(freq)) {

      restab <- restab[-grep("Freq", restab[, 3]), ]

    }

    # No percentages
    if (isTRUE(print == "no")) {

      restab <- restab[-grep(" %", restab[, 3]), -3]

     } else {

      # No total percentages
      if ("total" %in% no.perc) {

        restab <- restab[-grep("Tot %", restab[, 3]), ]

      }

      # No row percentages
      if ("row" %in% no.perc) {

        restab <- restab[-grep("Row %", restab[, 3]), ]

      }

      # No col percentages
      if ("col" %in% no.perc) {

        restab <- restab[-grep("Col %", restab[, 3]), ]

      }

    }

    #......
    # Format

    if (any(print == "no")) {

      #......
      # Variable names
      restab <- rbind(c(rep("", times = 2), colnames(x$data)[3], rep("", times = (ncol(restab) - 3))),
                      c(colnames(x$dat)[1], colnames(x$data)[2], row.names(print.object$freq.a[[1]]), "Total"),
                      restab,
                      c("Total", "", apply(freq.a.print, 2, sum), sum(freq.a.print)))

      # Justify right
      restab[-1, ] <- apply(restab[-1, ], 2, function(y) format(y, justify = "right"))

      # First variable
      if (nchar(colnames(x$data)[1]) < max(nchar(restab[, 1]))) {

        restab[2, 1] <- paste(sub("^\\s+", "", restab[2, 1]), paste(rep(" ", times = max(nchar(restab[, 1])) - nchar(colnames(x$data)[1]) - 1) , collapse = ""))

      }

      if (nchar(colnames(x$data)[1]) == max(nchar(restab[, 1]))) {

        restab[2, 1] <- paste(sub("^\\s+", "", restab[2, 1]), paste(rep(" ", times = max(nchar(restab[, 1])) - nchar(colnames(x$data)[1])) , collapse = ""))

      }

      # Second variable
      if (nchar(colnames(x$data)[2]) < max(nchar(restab[, 2]))) {

        restab[2, 2] <- paste(sub("^\\s+", "", restab[2, 2]), paste(rep(" ", times = max(nchar(restab[, 2])) - nchar(colnames(x$data)[2]) - 1), collapse = ""))

      }

      if (nchar(colnames(x$data)[2]) == max(nchar(restab[, 2]))) {

        restab[2, 2] <- paste(sub("^\\s+", "", restab[2, 2]), paste(rep(" ", times = max(nchar(restab[, 2])) - nchar(colnames(x$data)[2])), collapse = ""))

      }

      # Format
      restab[, 1:2] <- apply(restab[, 1:2], 2, format)

      restab[-2, 1] <- paste0(" ", restab[-2, 1])
      restab[-2, 2] <- paste0(" ", restab[-2, 2])
      restab[-1, 3] <- paste0(" ", restab[-1, 3])

      restab[, 1:2] <- apply(restab[, 1:2], 2, format)

      restab[, 1] <- paste0(" ", restab[, 1])

    # Percentage(s)
    } else {

      #......
      # Variable names
      restab <- rbind(c(rep("", times = 3), colnames(x$data)[3], rep("", times = (ncol(restab) - 4))),
                      c(colnames(x$dat)[1], colnames(x$data)[2], "", row.names(print.object$freq.a[[1]]), "Total"),
                      restab,
                      c("Total", "", "", apply(freq.a.print, 2, sum), sum(freq.a.print)))

      # Format percentages
      restab[grep("%", restab[, 3]), -c(1:3, ncol(restab))] <- apply(restab[grep("%", restab[, 3]), -c(1:3, ncol(restab))], 2, function(y) paste0(format(formatC(as.numeric(y), digits = digits, format = "f"), justify = "right"), "%"))

      # Format variable names
      restab[2, 1] <- format(restab[2, 1], justify = "left", width = max(nchar(restab[, 1])))
      restab[2, 2] <- format(restab[2, 2], justify = "left", width = max(nchar(restab[, 2])))

      # Format values
      restab[-2, 1] <- format(restab[-2, 1], justify = "right", width = max(nchar(restab[, 1])))
      restab[-2, 2] <- format(restab[-2, 2], justify = "right", width = max(nchar(restab[, 2])))

      # Justify right
      restab[-1, -c(1:3)] <- apply(restab[-1, -c(1:3)], 2, function(y) format(y, justify = "right"))

      restab[, 3] <- format(restab[, 3], justify = "left")

      # First variable
      restab[-2, 1] <- paste0("  ", restab[-2, 1])
      restab[2, 1] <- paste0(" ", restab[2, 1], " ")

      # Second variable
      restab[-2, 2] <- paste0(" ", restab[-2, 2])
      restab[2, 2] <- paste0(restab[2, 2], " ")

      # Third variable
      restab[-1, 4] <- paste0(" ", restab[-1, 4])

    }

    #......
    # Output table not split
    if (isFALSE(split) | all(print == "no")) {

      # Remove Total row and column
      if (isFALSE(freq)) {

        restab <- restab[-nrow(restab), -ncol(restab)]

      }

      # Remove duplicated row labels
      restab[-c(1:2, nrow(restab)), 2] <- unlist(tapply(restab[-c(1:2, nrow(restab)), 2], restab[-c(1:2, nrow(restab)), 1], function(y) ifelse(duplicated(y), paste0(rep(" ", times = unique(nchar(restab[, 2]))), collapse = ""), restab[-c(1:2, nrow(restab)), 2])))
      restab[, 1] <- ifelse(duplicated(restab[, 1]), paste(rep(" ", times = unique(nchar(restab[, 1]))), collapse = ""), restab[, 1])

      # Print results
      write.table(restab, col.names = FALSE, row.names = FALSE, quote = FALSE)

    # Output table split
    } else {

      if (isTRUE(freq)) {

        # Frequencies
        restab.abs <- restab[-grep("%", restab[, 3]), -3]

        restab.abs[-1, -1] <- apply(restab.abs[-1, -1], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.abs <- rbind(c(paste(rep(" ", times = max(nchar(restab.abs[, 1]))), collapse = ""),
                              paste(rep(" ", times = max(nchar(restab.abs[, 2]))), collapse = ""),
                                 colnames(x$data)[3], rep("", times = ncol(restab.abs) - 3)),
                            restab.abs[-1, ])

        restab.abs[-1, 3] <- paste0(" ",restab.abs[-1, 3])
        restab.abs[1, 3] <- paste0(restab.abs[1, 3], " ")

        restab.abs[, 1] <- paste0(" ", restab.abs[, 1])

        cat(" Frequencies\n")
        write.table(restab.abs, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Row-wise percentages
      if ("row" %in% print) {

        if (isTRUE(freq)) { cat("\n") }

        restab.row <- cbind(rbind(restab[1:2, -c(3, ncol(restab))], restab[grep("Row", restab[, 3]), -c(3, ncol(restab))]),
                            c("", "Total",
                              rep(ifelse(digits == 0, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = nrow(restab[grep("Row", restab[, 3]), ]))))

        restab.row[which(apply(restab.row, 1, function(y) length(grep("NA%", y)) != 0)), ncol(restab.row)] <- "NA%"

        restab.row[, ncol(restab.row)] <- format(restab.row[, ncol(restab.row)], justify = "right")

        restab.row[-1, -1] <- apply(restab.row[-1, -1], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.row <- rbind(c(paste(rep(" ", times = max(nchar(restab.row[, 1]))), collapse = ""),
                              paste(rep(" ", times = max(nchar(restab.row[, 2]))), collapse = ""), colnames(x$data)[3], rep("", times = ncol(restab.row) - 3)),
                            restab.row[-1, ])

        restab.row[1, 3] <- format(restab.row[1, 3], justify = "left", width = max(nchar(restab.row[, 3])))

        restab.row[-1, 3] <- paste0(" ",restab.row[-1, 3])
        restab.row[1, 3] <- paste0(restab.row[1, 3], " ")

        restab.row[, 1] <- paste0(" ", restab.row[, 1])

        cat("Row-Wise Percentages\n")
        write.table(restab.row, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Column-wise percentages
      if ("col" %in% print) {

        if (isTRUE(freq) | "row" %in% print) { cat("\n") }

        restab.col <- restab[grep("Col", restab[, 3]), -c(3, ncol(restab))]

        p <- c(paste(paste(rep(" ", times = max(nchar(restab.col[, 1])) - 6), collapse = ""), "Total", collapse = ""), "",
                     rep(ifelse(digits == 0, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = ncol(restab.col) - 2))

        restab.col.p <- NULL
        for (i in unique(restab.col[, 1])) {

          temp <- rbind(restab.col[restab.col[, 1] == i, ], p)
          temp[nrow(temp), which(apply(temp, 2, function(y) length(grep("NA%", y)) != 0))] <- "NA%"

          restab.col.p  <- rbind(restab.col.p, temp)

        }

        restab.col <- rbind(restab[1:2, -c(3, ncol(restab))], restab.col.p)

        restab.col[-1, -1] <- apply(restab.col[-1, -1], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.col <- rbind(c(paste0(rep(" ", times = max(nchar(restab.col[, 1]))), collapse = ""),
                              paste0(rep(" ", times = max(nchar(restab.col[, 2]))), collapse = ""), colnames(x$data)[3], rep("", times = ncol(restab.col) - 3)),
                             restab.col[-1, ])

        restab.col[-1, 3] <- paste0(" ", restab.col[-1, 3])
        restab.col[1, 3] <- paste0(restab.col[1, 3], " ")

        restab.col[, 1] <- paste0(" ", restab.col[, 1])

        cat(" Column-Wise Percentages\n")
        write.table(restab.col, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Total percentages
      if ("total" %in% print) {

        if (isTRUE(freq) | "row" %in% print | "col" %in% print) { cat("\n") }

        restab.total <- rbind(restab[1:2, -c(3, ncol(restab))], restab[grep("Tot", restab[, 3]), -c(3, ncol(restab))])
        restab.total <- rbind(cbind(restab.total, c("", "Total", rep("", times = nrow(restab.total) - 2))), c(rep("", times = ncol(restab.total)),
                                                                                                              ifelse(digits == 0, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%"))))
        restab.total[, ncol(restab.total)] <- format(restab.total[, ncol(restab.total)], justify = "right")
        restab.total <- apply(restab.total, 2, format)

        restab.total[-1, -1] <- apply(restab.total[-1, -1], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.total <- rbind(c(paste(rep(" ", times = max(nchar(restab.total[, 1]))), collapse = ""),
                                paste(rep(" ", times = max(nchar(restab.total[, 2]))), collapse = ""), colnames(x$data)[3], rep("", times = ncol(restab.total) - 3)),
                              restab.total[-1, ])

        restab.total[-1, 3] <- paste0(" ", restab.total[-1, 3])
        restab.total[1, 3] <- paste0(restab.total[1, 3], " ")

        restab.total[, 1] <- paste0(" ", restab.total[, 1])

        cat(" Total Percentages\n")
        write.table(restab.total, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

    }

  }

}
