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
#'                   x3 = c(1, 2, 1, 1, 1, 2, 2, 2, 2, 1), stringsAsFactors = FALSE)
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
    if (isFALSE(isTRUE(freq) || isFALSE(freq))) {

      stop("Please specify TRUE or FALSE for the argument 'freq'.", call. = FALSE)

    }

    #......
    # Check print = "no" and freq = FALSE
    if (print == "no" && isFALSE(freq)) {

      stop("Please include either percentages (i.e., print != 'no') or absolute frequencies (i.e., freq = TRUE) in the cross tabulation.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L || digits < 0L) {

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

  if (ncol(x$data) == 2L) {

    restab <- cbind(rep(names(print.object$freq.a[, 1L]), times = 4L),
                    rep(c("Freq", "Row %", "Col %", "Tot %"), each = nrow(print.object$freq.a)),
                    rbind(print.object$freq.a, print.object$perc.r, print.object$perc.c, print.object$perc.t),
                    c(apply(print.object$freq.a, 1, sum), rep("", times = 3*nrow(print.object$freq.a))))

    #......
    # Sort table

    # First variable is a factor
    if (is.factor(x$data[, 1L])) {

      # Sort with NA
      if (any(is.na(x$data)) && isFALSE(x$args$na.omit)) {

        restab <- restab[order(factor(restab[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA"))), ]

      # Sort without NA
      } else {

        restab <- restab[order(factor(restab[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L]))), ]

      }

    # First variable is not a factor
    } else {

      restab <- restab[order(restab[, 1L]), ]

    }

    #......
    # Frequencies and percentages

    # No absolute frequencies
    if (isFALSE(freq)) {

      restab <- restab[-grep("Freq", restab[, 2L]), ]

    }

    # No percentages
    if (any(print == "no")) {

      restab <- restab[-grep(" %", restab[, 2L]), -2L]

    } else {

      # No total percentages
      if ("total" %in% no.perc) {

        restab <- restab[-grep("Tot %", restab[, 2]), ]

      }

      # No row percentages
      if ("row" %in% no.perc) {

        restab <- restab[-grep("Row %", restab[, 2L]), ]

      }

      # No col percentages
      if ("col" %in% no.perc) {

        restab <- restab[-grep("Col %", restab[, 2L]), ]

      }

    }

    #......
    # Format

    # Frequencies only
    if (any(print == "no")) {

      #......
      # Variable names and column sum
      restab <- rbind(c("", colnames(x$data)[2], rep("", times = (ncol(restab) - 2L))),
                      c(colnames(x$data)[1], colnames(restab)[-c(1L, ncol(restab))], "Total"),
                      restab,
                      c("Total", apply(print.object$freq.a, 2, sum), sum(print.object$freq.a)))

      # Format
      restab[2L, 1L] <- paste0(" ", restab[2L, 1L])
      restab[-c(1L, 2L), 1L] <- paste0("  ", restab[-c(1L, 2L), 1L])

      # Justify right
      restab[-2L, 1L] <- format(restab[-2L, 1L], justify = "right")
      restab[, 1L] <- format(restab[, 1L])

      restab[-1L, -1L] <- apply(restab[-1L, -1L], 2, function(y) format(y, justify = "right"))

      restab[-1L, 2L] <- paste0(" ", restab[-1L, 2L])

    # Percentage(s)
    } else {

      #......
      # Variable names and column sum
      restab <- rbind(c("", "", colnames(x$data)[2L], rep("", times = (ncol(restab) - 3L))),
                      c(colnames(x$data)[1], colnames(restab)[-c(1L, ncol(restab))], "Total"),
                      restab,
                      c("Total", "", apply(print.object$freq.a, 2, sum), sum(print.object$freq.a)))

      # Format percentages
      restab[grep("%", restab[, 2L]), -c(1L:2L, ncol(restab))] <- apply(restab[grep("%", restab[, 2L]), -c(1L:2L, ncol(restab))], 2, function(y) paste0(format(formatC(as.numeric(y), digits = digits, format = "f"), justify = "right"), "%"))

      restab <- gsub("NaN", "NA", restab)

      # Justify right and left
      restab[, 1L] <- format(restab[, 1L], justify = "right")

      restab[2L, 1L] <- paste0(" ", sub("^\\s+", "", restab[2L, 1L]), paste0(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L])), collapse = ""), " ")
      restab[-2L, 1L] <- paste0("  ", restab[-2L, 1L])

      restab[, 2L] <- format(restab[, 2L], justify = "left")

      restab[-1L, -c(1L:2L)] <- apply(restab[-1L, -c(1L:2L)], 2, format, justify = "right")

      restab[-1L, 3L] <- paste0(" ", restab[-1L, 3L])

    }

    #......
    # Output table not split
    if (isFALSE(split) || all(print == "no")) {

      # Remove Total row and column
      if (isFALSE(freq)) {

        restab <- restab[-nrow(restab), -ncol(restab)]

      }

      # Remove duplicated row labels
      restab[, 1L] <- ifelse(duplicated(restab[, 1L]), paste(rep(" ", times = unique(nchar(restab[, 1L]))), collapse = ""), restab[, 1L])

      # Print results
      write.table(restab, col.names = FALSE, row.names = FALSE, quote = FALSE)

    # Output table split
    } else {

      if (isTRUE(freq)) {

        # Frequencies
        restab.abs <- restab[-grep("%", restab[, 2L]), -2L]

        restab.abs[-1L, -1L] <- apply(restab.abs[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.abs[-1L, 2L] <- paste0(" ", restab.abs[-1L, 2L])

        restab.abs[, 1L] <- paste0(" ", restab.abs[, 1L])

        cat(" Frequencies\n")
        write.table(restab.abs, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Row-wise percentages
      if ("row" %in% print) {

        if (isTRUE(freq)) { cat("\n") }

        restab.row <- cbind(rbind(restab[1L:2L, -c(2L, ncol(restab))], restab[grep("Row", restab[, 2L]), -c(2L, ncol(restab))]),
                            c("", "Total",
                              rep(ifelse(digits == 0L, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = nrow(restab[grep("Row", restab[, 2L]), ]))))

        restab.row[which(apply(restab.row, 1, function(y) length(grep("NA%", y)) != 0L)), ncol(restab.row)] <- "NA%"

        restab.row[, ncol(restab.row)] <- format(restab.row[, ncol(restab.row)], justify = "right")

        restab.row[-1L, -1L] <- apply(restab.row[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.row[-1L, 2L] <- paste0(" ", restab.row[-1L, 2L])

        restab.row[, 1L] <- paste0(" ", restab.row[, 1L])

        cat(" Row-Wise Percentages\n")
        write.table(restab.row, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Column-wise percentages
      if ("col" %in% print) {

        if (isTRUE(freq) || "row" %in% print) { cat("\n") }

        restab.col <- rbind(restab[1L:2L, -c(2L, ncol(restab))], restab[grep("Col", restab[, 2]), -c(2L, ncol(restab))],
                            c("Total", rep(ifelse(digits == 0L, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = ncol(restab) - 3L)))

        restab.col[nrow(restab.col), which(apply(restab.col, 2, function(y) length(grep("NA%", y)) != 0L))] <- "NA%"

        restab.col[-1L, ] <- apply(restab.col[-1L, ], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.col[, 1L] <- format(restab.col[, 1L])

        restab.col[-1L, 2L] <- paste0(" ", restab.col[-1L, 2L])

        restab.col[, 1L] <- paste0("  ", restab.col[, 1L])

        cat(" Column-Wise Percentages\n")
        write.table(restab.col, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Total percentages
      if ("total" %in% print) {

        if (isTRUE(freq) || "row" %in% print || "col" %in% print) { cat("\n") }

        restab.total <- rbind(restab[1L:2L, -c(2L, ncol(restab))], restab[grep("Tot", restab[, 2L]), -c(2L, ncol(restab))])

        restab.total[-1L, -1L] <- apply(restab.total[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.total <- rbind(cbind(restab.total, c("", "Total", rep("", times = nrow(restab.total) - 2L))), c(rep("", times = ncol(restab.total)),
                                                                                                             ifelse(digits == 0, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%"))))

        restab.total[, ncol(restab.total)] <- format(restab.total[, ncol(restab.total)], justify = "right")

        restab.total[-1L, ] <- apply(restab.total[-1L, ], 2, format)

        restab.total[-1L, 2L] <- paste0(" ", restab.total[-1L, 2L])

        restab.total[, 1L] <- paste0(" ", restab.total[, 1L])

        cat(" Total Percentages\n")
        write.table(restab.total, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

    }

  }

  #----------------------------------------
  # Three-Dimensional Matrix

  if (ncol(x$data) == 3L) {

    # Absolute frequencies
    freq.a.print <- NULL
    for (i in seq_len(nrow(print.object$freq.a[[1]]))) {

      freq.a.print <- cbind(freq.a.print,
                            unlist(lapply(print.object$freq.a, function(y) y[i, ])))

    }

    # Row %
    perc.r.print <- NULL
    for (i in seq_len(nrow(print.object$perc.c[[1L]]))) {

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
    restab <- cbind(rep(names(print.object$freq.a), each = ncol(print.object$freq.a[[1]]), time = 4L),
                    rep(colnames(print.object$freq.a[[1L]]), times = 4L*length(print.object$freq.a)),
                    rep(c("Freq", "Row %", "Col %", "Tot %"), each =  ncol(print.object$freq.a[[1L]])*length(print.object$freq.a)),
                    rbind(freq.a.print, perc.r.print, perc.c.print, perc.t.print),
                    c(apply(freq.a.print, 1, sum), rep("", times = 3L*length(print.object$freq.a)*ncol(print.object$freq.a[[1]]))))

    # Convert NaN in NA
    restab <- gsub("NaN", NA, restab)

    #......
    # Sort table

    # First and scond variable are a factor
    if (is.factor(x$data[, 1L]) && is.factor(x$data[, 2L])) {

      # Sort with NA
      if (any(is.na(x$data)) && isFALSE(x$args$na.omit)) {

        restab <-restab[order(factor(restab[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA")),
                        factor(restab[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

      # Sort without NA
      } else {

        restab <- restab[order(factor(restab[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L])),
                               factor(restab[, 2L], levels = levels(x$data[, 2L]), labels = levels(x$data[, 2L]))), ]

      }

    }

    # First variable is a factor, second variable is not a factor
    if (is.factor(x$data[, 1L]) && !is.factor(x$data[, 2L])) {

      # Sort with NA
      if (any(is.na(x$data)) && isFALSE(x$args$na.omit)) {

        restab <- restab[order(factor(restab[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA")),
                               restab[, 2L]), ]

      # Sort without NA
      } else {

        restab <- restab[order(factor(restab[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L])),
                               restab[, 2L]), ]

      }

    }

    # First variable is not a factor, second variable is a factor
    if (!is.factor(x$data[, 1L]) && is.factor(x$data[, 2L])) {

      # Sort with NA
      if (any(is.na(x$data)) && isFALSE(x$args$na.omit)) {

        restab <- restab[order(restab[, 1L],
                               factor(restab[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

      # Sort without NA
      } else {

        restab <- restab[order(restab[, 1L],
                               factor(restab[, 2L], levels = levels(x$data[, 2L]), labels = levels(x$data[, 2L]))), ]

      }

    }

    # First and second variable are not a factor
    if (!is.factor(x$data[, 1L]) && !is.factor(x$data[, 2L])) {

      # Sort with NA
      if (any(is.na(x$data)) && isFALSE(x$args$na.omit)) {

        restab <- restab[order(restab[, 1L],
                               factor(restab[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

        # Sort without NA
      } else {

        restab <- restab[order(restab[, 1L], restab[, 2L]), ]

      }

    }

    #......
    # Frequencies and percentages

    # No absolute frequencies
    if (isFALSE(freq)) {

      restab <- restab[-grep("Freq", restab[, 3L]), ]

    }

    # No percentages
    if (isTRUE(print == "no")) {

      restab <- restab[-grep(" %", restab[, 3L]), -3L]

     } else {

      # No total percentages
      if ("total" %in% no.perc) {

        restab <- restab[-grep("Tot %", restab[, 3L]), ]

      }

      # No row percentages
      if ("row" %in% no.perc) {

        restab <- restab[-grep("Row %", restab[, 3L]), ]

      }

      # No col percentages
      if ("col" %in% no.perc) {

        restab <- restab[-grep("Col %", restab[, 3L]), ]

      }

    }

    #......
    # Format

    if (any(print == "no")) {

      #......
      # Variable names
      restab <- rbind(c(rep("", times = 2L), colnames(x$data)[3L], rep("", times = (ncol(restab) - 3L))),
                      c(colnames(x$dat)[1L], colnames(x$data)[2L], row.names(print.object$freq.a[[1L]]), "Total"),
                      restab,
                      c("Total", "", apply(freq.a.print, 2, sum), sum(freq.a.print)))

      # Justify right
      restab[-1L, ] <- apply(restab[-1L, ], 2, function(y) format(y, justify = "right"))

      # First variable
      if (nchar(colnames(x$data)[1]) < max(nchar(restab[, 1L]))) {

        restab[2L, 1L] <- paste(sub("^\\s+", "", restab[2L, 1L]), paste(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L]) - 1L) , collapse = ""))

      }

      if (nchar(colnames(x$data)[1L]) == max(nchar(restab[, 1L]))) {

        restab[2L, 1L] <- paste(sub("^\\s+", "", restab[2L, 1L]), paste(rep(" ", times = max(nchar(restab[, 1L])) - nchar(colnames(x$data)[1L])) , collapse = ""))

      }

      # Second variable
      if (nchar(colnames(x$data)[2L]) < max(nchar(restab[, 2L]))) {

        restab[2L, 2L] <- paste(sub("^\\s+", "", restab[2L, 2L]), paste(rep(" ", times = max(nchar(restab[, 2L])) - nchar(colnames(x$data)[2L]) - 1L), collapse = ""))

      }

      if (nchar(colnames(x$data)[2L]) == max(nchar(restab[, 2L]))) {

        restab[2L, 2L] <- paste(sub("^\\s+", "", restab[2L, 2L]), paste(rep(" ", times = max(nchar(restab[, 2L])) - nchar(colnames(x$data)[2L])), collapse = ""))

      }

      # Format
      restab[, 1L:2L] <- apply(restab[, 1L:2L], 2, format)

      restab[-2L, 1L] <- paste0(" ", restab[-2L, 1L])
      restab[-2L, 2L] <- paste0(" ", restab[-2L, 2L])
      restab[-1L, 3L] <- paste0(" ", restab[-1L, 3L])

      restab[, 1L:2L] <- apply(restab[, 1L:2L], 2, format)

      restab[, 1L] <- paste0(" ", restab[, 1L])

    # Percentage(s)
    } else {

      #......
      # Variable names
      restab <- rbind(c(rep("", times = 3L), colnames(x$data)[3L], rep("", times = (ncol(restab) - 4L))),
                      c(colnames(x$dat)[1L], colnames(x$data)[2L], "", row.names(print.object$freq.a[[1L]]), "Total"),
                      restab,
                      c("Total", "", "", apply(freq.a.print, 2, sum), sum(freq.a.print)))

      # Format percentages
      restab[grep("%", restab[, 3L]), -c(1:3, ncol(restab))] <- apply(restab[grep("%", restab[, 3L]), -c(1L:3L, ncol(restab))], 2, function(y) paste0(format(formatC(as.numeric(y), digits = digits, format = "f"), justify = "right"), "%"))

      # Format variable names
      restab[2L, 1L] <- format(restab[2L, 1L], justify = "left", width = max(nchar(restab[, 1L])))
      restab[2L, 2L] <- format(restab[2L, 2L], justify = "left", width = max(nchar(restab[, 2L])))

      # Format values
      restab[-2L, 1L] <- format(restab[-2L, 1L], justify = "right", width = max(nchar(restab[, 1L])))
      restab[-2L, 2L] <- format(restab[-2L, 2L], justify = "right", width = max(nchar(restab[, 2L])))

      # Justify right
      restab[-1L, -c(1L:3L)] <- apply(restab[-1L, -c(1L:3L)], 2, function(y) format(y, justify = "right"))

      restab[, 3L] <- format(restab[, 3L], justify = "left")

      # First variable
      restab[-2L, 1L] <- paste0("  ", restab[-2L, 1L])
      restab[2L, 1L] <- paste0(" ", restab[2L, 1L], " ")

      # Second variable
      restab[-2L, 2L] <- paste0(" ", restab[-2L, 2L])
      restab[2L, 2L] <- paste0(restab[2L, 2L], " ")

      # Third variable
      restab[-1L, 4L] <- paste0(" ", restab[-1L, 4L])

    }

    #......
    # Output table not split
    if (isFALSE(split) || all(print == "no")) {

      # Remove Total row and column
      if (isFALSE(freq)) {

        restab <- restab[-nrow(restab), -ncol(restab)]

      }

      # Remove duplicated row labels
      restab[-c(1L:2L, nrow(restab)), 2L] <- unlist(tapply(restab[-c(1L:2L, nrow(restab)), 2L], restab[-c(1L:2L, nrow(restab)), 1], function(y) ifelse(duplicated(y), paste0(rep(" ", times = unique(nchar(restab[, 2]))), collapse = ""), restab[-c(1L:2L, nrow(restab)), 2L])))
      restab[, 1L] <- ifelse(duplicated(restab[, 1L]), paste(rep(" ", times = unique(nchar(restab[, 1L]))), collapse = ""), restab[, 1])

      # Print results
      write.table(restab, col.names = FALSE, row.names = FALSE, quote = FALSE)

    # Output table split
    } else {

      if (isTRUE(freq)) {

        # Frequencies
        restab.abs <- restab[-grep("%", restab[, 3L]), -3L]

        restab.abs[-1L, -1L] <- apply(restab.abs[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.abs <- rbind(c(paste(rep(" ", times = max(nchar(restab.abs[, 1L]))), collapse = ""),
                              paste(rep(" ", times = max(nchar(restab.abs[, 2L]))), collapse = ""),
                                 colnames(x$data)[3L], rep("", times = ncol(restab.abs) - 3L)),
                            restab.abs[-1L, ])

        restab.abs[-1L, 3L] <- paste0(" ",restab.abs[-1L, 3L])
        restab.abs[1L, 3L] <- paste0(restab.abs[1L, 3L], " ")

        restab.abs[, 1L] <- paste0(" ", restab.abs[, 1L])

        cat(" Frequencies\n")
        write.table(restab.abs, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Row-wise percentages
      if ("row" %in% print) {

        if (isTRUE(freq)) { cat("\n") }

        restab.row <- cbind(rbind(restab[1L:2L, -c(3L, ncol(restab))], restab[grep("Row", restab[, 3L]), -c(3L, ncol(restab))]),
                            c("", "Total",
                              rep(ifelse(digits == 0L, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = nrow(restab[grep("Row", restab[, 3L]), ]))))

        restab.row[which(apply(restab.row, 1, function(y) length(grep("NA%", y)) != 0L)), ncol(restab.row)] <- "NA%"

        restab.row[, ncol(restab.row)] <- format(restab.row[, ncol(restab.row)], justify = "right")

        restab.row[-1L, -1L] <- apply(restab.row[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.row <- rbind(c(paste(rep(" ", times = max(nchar(restab.row[, 1L]))), collapse = ""),
                              paste(rep(" ", times = max(nchar(restab.row[, 2L]))), collapse = ""), colnames(x$data)[3L], rep("", times = ncol(restab.row) - 3L)),
                            restab.row[-1L, ])

        restab.row[1L, 3L] <- format(restab.row[1L, 3L], justify = "left", width = max(nchar(restab.row[, 3L])))

        restab.row[-1L, 3L] <- paste0(" ",restab.row[-1L, 3L])
        restab.row[1L, 3L] <- paste0(restab.row[1L, 3L], " ")

        restab.row[, 1L] <- paste0(" ", restab.row[, 1L])

        cat("Row-Wise Percentages\n")
        write.table(restab.row, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Column-wise percentages
      if ("col" %in% print) {

        if (isTRUE(freq) || "row" %in% print) { cat("\n") }

        restab.col <- restab[grep("Col", restab[, 3]), -c(3, ncol(restab))]

        p <- c(paste(paste(rep(" ", times = max(nchar(restab.col[, 1L])) - 6L), collapse = ""), "Total", collapse = ""), "",
                     rep(ifelse(digits == 0, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%")), times = ncol(restab.col) - 2L))

        restab.col.p <- NULL
        for (i in unique(restab.col[, 1L])) {

          temp <- rbind(restab.col[restab.col[, 1L] == i, ], p)
          temp[nrow(temp), which(apply(temp, 2, function(y) length(grep("NA%", y)) != 0L))] <- "NA%"

          restab.col.p  <- rbind(restab.col.p, temp)

        }

        restab.col <- rbind(restab[1L:2L, -c(3L, ncol(restab))], restab.col.p)

        restab.col[-1L, -1L] <- apply(restab.col[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.col <- rbind(c(paste0(rep(" ", times = max(nchar(restab.col[, 1L]))), collapse = ""),
                              paste0(rep(" ", times = max(nchar(restab.col[, 2L]))), collapse = ""), colnames(x$data)[3L], rep("", times = ncol(restab.col) - 3L)),
                             restab.col[-1L, ])

        restab.col[-1L, 3L] <- paste0(" ", restab.col[-1L, 3L])
        restab.col[1L, 3L] <- paste0(restab.col[1L, 3L], " ")

        restab.col[, 1L] <- paste0(" ", restab.col[, 1L])

        cat(" Column-Wise Percentages\n")
        write.table(restab.col, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

      # Total percentages
      if ("total" %in% print) {

        if (isTRUE(freq) || "row" %in% print || "col" %in% print) { cat("\n") }

        restab.total <- rbind(restab[1L:2L, -c(3L, ncol(restab))], restab[grep("Tot", restab[, 3L]), -c(3L, ncol(restab))])
        restab.total <- rbind(cbind(restab.total, c("", "Total", rep("", times = nrow(restab.total) - 2L))), c(rep("", times = ncol(restab.total)),
                                                                                                              ifelse(digits == 0L, "100", paste0("100.", paste(rep("0", times = digits), collapse = ""), "%"))))
        restab.total[, ncol(restab.total)] <- format(restab.total[, ncol(restab.total)], justify = "right")
        restab.total <- apply(restab.total, 2, format)

        restab.total[-1L, -1L] <- apply(restab.total[-1L, -1L], 2, function(y) formatC(sub("^\\s+", "", y), format = "f"))

        restab.total <- rbind(c(paste(rep(" ", times = max(nchar(restab.total[, 1L]))), collapse = ""),
                                paste(rep(" ", times = max(nchar(restab.total[, 2L]))), collapse = ""), colnames(x$data)[3L], rep("", times = ncol(restab.total) - 3L)),
                              restab.total[-1L, ])

        restab.total[-1L, 3L] <- paste0(" ", restab.total[-1L, 3L])
        restab.total[1L, 3L] <- paste0(restab.total[1L, 3L], " ")

        restab.total[, 1L] <- paste0(" ", restab.total[, 1L])

        cat(" Total Percentages\n")
        write.table(restab.total, col.names = FALSE, row.names = FALSE, quote = FALSE)

      }

    }

  }

}
