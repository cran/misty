#' Write Results of a misty Object into an Excel file
#'
#' This function writes the results of a misty object (\code{misty.object})
#' into a Excel file.
#'
#' Currently the function supports result objects from the function
#' \code{cor.matrix}, \code{crosstab}, \code{freq}, \code{item.alpha},
#' \code{item.alpha}, \code{item.cfa}, \code{item.omega}, \code{multilevel.cor},
#' \code{multilevel.descript}, \code{na.coverage}, \code{na.descript}, and
#' \code{na.pattern}.
#'
#'
#' @param x    misty object (\code{misty.object}) resulting from a misty function
#'             supported by the \code{write.result} function (see 'Details').
#' @param file a character string naming a file with or without file extension
#'             '.xlsx', e.g., \code{"Results.xlsx"} or \code{"Results"}.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cor.matrix}}, \code{\link{crosstab}}, \code{\link{freq}},
#' \code{\link{item.alpha}}, \code{\link{item.cfa}}, \code{\link{item.omega}},
#' \code{\link{multilevel.cor}}, \code{\link{multilevel.descript}},
#' \code{\link{na.coverage}}, \code{\link{na.descript}}, \code{\link{na.pattern}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' #--------------------------------------
#' # cor.matrix() function
#'
#' result <- cor.matrix(mtcars, print = "all", output = FALSE)
#' write.result(result, "Correlation.xlsx")
#'
#' #--------------------------------------
#' # crosstab() function
#'
#' result <- crosstab(mtcars[, c("carb", "gear")], print = "all", output = FALSE)
#' write.result(result, "Crosstab.xlsx")
#'
#' #--------------------------------------
#' # descript() function
#'
#' result <- descript(mtcars, output = FALSE)
#' write.result(result, "Descript.xlsx")
#'
#' #--------------------------------------
#' # freq() function
#'
#' result <- freq(mtcars, exclude = 99, output = FALSE)
#' write.result(result, "Freq.xlsx")
#'
#' #--------------------------------------
#' # item.alpha() function
#'
#' result <- item.alpha(attitude, output = FALSE)
#' write.result(result, "Alpha.xlsx")
#'
#' #--------------------------------------
#' # item.cfa() function
#'
#' # Load data set "HolzingerSwineford1939" in the lavaan package
#' data("HolzingerSwineford1939", package = "lavaan")
#'
#' result <- item.cfa(HolzingerSwineford1939[, c("x1", "x2", "x3")],
#'                    output = FALSE)
#' write.result(result, "CFA.xlsx")
#'
#' #--------------------------------------
#' # item.omega() function
#'
#' result <- item.omega(attitude, output = FALSE)
#' write.result(result, "Omega.xlsx")
#'
#' #--------------------------------------
#' # multilevel.cor() function
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' result <- multilevel.cor(Demo.twolevel[, c("y1", "y2", "y3")],
#'                          cluster = Demo.twolevel$cluster, output = FALSE)
#' write.result(result, "Multilevel_Correlation.xlsx")
#'
#' #--------------------------------------
#' # multilevel.descript() function
#'
#' # Load data set "Demo.twolevel" in the lavaan package
#' data("Demo.twolevel", package = "lavaan")
#'
#' result <- multilevel.descript(Demo.twolevel[, c("y1", "y2", "y3")],
#'                               cluster = Demo.twolevel$cluster, output = FALSE)
#' write.result(result, "Multilevel_Descript.xlsx")
#'
#' #--------------------------------------
#' # na.coverage() function
#'
#' dat <- data.frame(x = c(1, NA, NA, 6, 3),
#'                   y = c(7, NA, 8, 9, NA),
#'                   z = c(2, NA, 3, NA, 5))
#'
#' result <- na.coverage(dat, output = FALSE)
#' write.result(result, "NA_Coverage.xlsx")
#'
#' #--------------------------------------
#' # na.descript() function
#'
#' dat <- data.frame(x1 = c(1, NA, 2, 5, 3, NA, 5, 2),
#'                   x2 = c(4, 2, 5, 1, 5, 3, 4, 5),
#'                   x3 = c(NA, 3, 2, 4, 5, 6, NA, 2),
#'                   x4 = c(5, 6, 3, NA, NA, 4, 6, NA))
#'
#'  result <- na.descript(dat, table = TRUE, output = FALSE)
#' write.result(result, "NA_Descriptives.xlsx")
#'
#' #--------------------------------------
#' # na.pattern() function
#'
#' dat <- data.frame(x = c(1, NA, NA, 6, 3),
#'                   y = c(7, NA, 8, 9, NA),
#'                   z = c(2, NA, 3, NA, 5))
#'
#' result <- na.pattern(dat, output = FALSE)
#' write.result(result, "NA_Pattern.xlsx")
#' }
write.result <- function(x, file = "Results.xlsx") {

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a misty object for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check if input 'x' is a misty object
  if (isTRUE(class(x) != "misty.object")) { stop("Please specify a misty object for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is supported by the function
  if (isTRUE(!x$type %in% c("cor.matrix", "crosstab", "descript", "freq", "item.alpha",
                            "item.cfa", "item.omega", "multilevel.cor", "multilevel.descript",
                            "na.coverage", "na.descript", "na.pattern"))) {

    stop("This type of misty object is not supported by the function.", call. = FALSE)

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Write object
  write.object <- x$result

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Correlation Matrix, cor.matrix() ####
  switch(x$type, cor.matrix = {

    # Add variable names in the rows
    write.object <- lapply(write.object, function(y) data.frame(colnames(y), y,
                                                                row.names = NULL, check.rows = FALSE,
                                                                check.names = FALSE, fix.empty.names = FALSE))

    # Add infos
    write.object$Info <- data.frame(c("Correlation coefficient:", "Missing data:", "Adjustment for multiple testing:"),
                                    c(switch(x$args$method, "pearson" = "Pearson Product-Moment",
                                                            "spearman" = "Spearman's Rank-Order",
                                                            "kendall-b" = "Kendall's Tau-b",
                                                            "kendall-c" = "Kendall-Stuart's Tau-c"),
                                      ifelse(isTRUE(attr(x$data, "missing")),
                                             ifelse(isTRUE(x$args$na.omit), "Listwise deletion", "Pairwise deletion"), "No missing data"),
                                      ifelse(x$args$p.adj == "none", "None", x$args$p.adj)),
                                      row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

    if (isTRUE(".group" %in% colnames(x$data))) {

      write.object$Info <- rbind(write.object$Info,
                                 c(paste0("Lower triangular: ", sort(unique(x$data$.group))[1L], ", Upper triangular: ", sort(unique(x$data$.group))[2L]), NA))

    }

    names(write.object) <- c("Cor", "n", "Stat", "df", "p", "Info")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Cross Tabulation, crosstab() ####
  }, crosstab = {

    #.......................
    # Two-Dimensional Matrix
    if (isTRUE(ncol(x$data) == 2L)) {

      write.object <- data.frame(rep(ifelse(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)",
                                                  x = names(write.object$freq.a[, 1L])), as.numeric(names(write.object$freq.a[, 1L])), names(write.object$freq.a[, 1L])), times = 4L),
                                 rep(c("Freq", "Row %", "Col %", "Tot %"), each = nrow(write.object$freq.a)),
                                 rbind(write.object$freq.a, write.object$perc.r, write.object$perc.c, write.object$perc.t),
                                 Total = c(rowSums(write.object$freq.a), rep(NA, times = 3L*nrow(write.object$freq.a))),
                                 row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

      #......
      # Sort table

      # First variable is a factor
      if (isTRUE(is.factor(x$data[, 1L]))) {

        # Sort with NA
        if (isTRUE(any(is.na(x$data)) && !isTRUE(x$args$na.omit))) {

          write.object <- write.object[order(factor(write.object[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA"))), ]

        # Sort without NA
        } else {

          write.object <- write.object[order(factor(write.object[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L]))), ]

        }

      # First variable is not a factor
      } else {

        write.object <- write.object[order(write.object [, 1L]), ]

      }

      # Add column sum
      write.object <- rbind(write.object,
                            c(NA, NA, colSums(write.object[write.object[, 2] == "Freq", -c(1L, 2L)])))

      write.object[nrow(write.object), 1L
                   ] <- "Total"

      #......
      # Output table not split
      if (!isTRUE(x$args$split)) {

        # Remove duplicated row labels
        write.object[, 1L] <- ifelse(duplicated(write.object[, 1L]), NA, write.object[, 1L])

        # Remove percentages
        if (isTRUE(x$args$print == "no")) {

          write.object <- data.frame(write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]) , 1L],
                                     write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]), -c(1L, 2L)],
                                     row.names = NULL, check.rows = FALSE,
                                     check.names = FALSE, fix.empty.names = FALSE)

        } else {

          if (isTRUE(!"row" %in% x$args$print)) {

            write.object <- write.object[-which(write.object[, 2L] == "Row %"), ]

          }

          if (isTRUE(!"col" %in% x$args$print)) {

            write.object <- write.object[-which(write.object[, 2L] == "Col %"), ]

          }

          if (isTRUE(!"total" %in% x$args$print)) {

            write.object <- write.object[-which(write.object[, 2L] == "Tot %"), ]

          }

        }

        # Add variable names
        write.object <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object) - 1L)),
                                   write.object,
                                   row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object)[2L] <- colnames(x$data)[2L]

      #......
      # Output table split
      } else {

        # Frequencies
        write.object.abs <- data.frame(write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]), 1L],
                                       write.object[write.object[, 2L] == "Freq" | is.na(write.object[, 2L]), -c(1L, 2L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Row-wise percentages
        write.object.row <- data.frame(write.object[which(write.object[, 2L] == "Row %"), 1L],
                                       write.object[which(write.object[, 2L] == "Row %"), -c(1L, 2L, ncol(write.object))],
                                       Total = rowSums(write.object[which(write.object[, 2L] == "Row %"), -c(1L, 2L, ncol(write.object))]),
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Column-wise percentages
        write.object.col <- data.frame(write.object[which(write.object[, 2L] == "Col %"), 1L],
                                       write.object[which(write.object[, 2L] == "Col %"), -c(1L, 2L, ncol(write.object))],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Add column sum
        write.object.col <- rbind(write.object.col,
                                  c(NA, colSums(write.object.col[, -1L])))

        write.object.col[nrow(write.object.col), 1] <- "Total"

        # Total percentages
        write.object.tot <- data.frame(write.object[write.object[, 2L] == "Tot %", 1L],
                                       write.object[write.object[, 2L] == "Tot %", -c(1L, 2L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        write.object.tot[nrow(write.object.tot), ncol(write.object.tot)] <- 100L

        #.............
        # Prepare list
        write.object <- list(Absolute = write.object.abs)

        if (isTRUE("row" %in% x$args$print)) {

          write.object$"Row%" <- write.object.row

        }

        if (isTRUE("col" %in% x$args$print)) {

          write.object$"Col%" <-write.object.col

        }

        if (isTRUE("total" %in% x$args$print)) {

          write.object$"Total%" <- write.object.tot

        }

      }

    #.......................
    # Three-Dimensional Matrix
    } else if (isTRUE(ncol(x$data) == 3L)) {

      # Absolute frequencies
      freq.a.write <- NULL
      for (i in seq_len(nrow(write.object$freq.a[[1L]]))) {

        freq.a.write <- cbind(freq.a.write,
                              unlist(lapply(write.object$freq.a, function(y) y[i, ])))

      }

      # Row %
      perc.r.write <- NULL
      for (i in seq_len(nrow(write.object$perc.c[[1L]]))) {

        perc.r.write <- cbind(perc.r.write,
                              unlist(lapply(write.object$perc.c, function(y) y[i, ])))

      }

      # Column %
      perc.c.write <- NULL
      for (i in seq_len(nrow(write.object$perc.r[[1L]]))) {

        perc.c.write <- cbind(perc.c.write,
                              unlist(lapply(write.object$perc.r, function(y) y[i, ])))

      }

      # Total %
      perc.t.write <- NULL
      for (i in seq_len(nrow(write.object$perc.t[[1L]]))) {

        perc.t.write <- cbind(perc.t.write,
                              unlist(lapply(write.object$perc.t, function(y) y[i, ])))

      }

      #......
      # Result table
      write.object  <- data.frame(rep(ifelse(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)",
                                                   x = names(write.object$freq.a)), as.numeric(names(write.object$freq.a)), names(write.object$freq.a)),
                                      each = ncol(write.object$freq.a[[1L]]), times = 4L),
                                  rep(ifelse(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)",
                                             x = colnames(write.object$freq.a[[1L]])), as.numeric(colnames(write.object$freq.a[[1L]])), colnames(write.object$freq.a[[1L]])),
                                      times = 4L*length(write.object$freq.a)),
                                  rep(c("Freq", "Row %", "Col %", "Tot %"), each =  ncol(write.object$freq.a[[1L]])*length(write.object$freq.a)),
                                  rbind(freq.a.write, perc.r.write, perc.c.write, perc.t.write),
                                  Total = c(apply(freq.a.write, 1L, sum), rep(NA, times = 3L*length(write.object$freq.a)*ncol(write.object$freq.a[[1L]]))),
                                  row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

      #......
      # Sort table

      # First and second variable are a factor
      if (isTRUE(is.factor(x$data[, 1L]) && is.factor(x$data[, 2L]))) {

        # Sort with NA
        if (isTRUE(any(is.na(x$data)) && !isTRUE(x$args$na.omit))) {

          write.object <- write.object[order(factor(write.object[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA")),
                                       factor(write.object[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

          # Sort without NA
        } else {

          write.object <- write.object[order(factor(write.object[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L])),
                                       factor(write.object[, 2L], levels = levels(x$data[, 2L]), labels = levels(x$data[, 2L]))), ]

        }

      # First variable is a factor, second variable is not a factor
      } else if (isTRUE(is.factor(x$data[, 1L]) && !is.factor(x$data[, 2L]))) {

        # Sort with NA
        if (isTRUE(any(is.na(x$data)) && !isTRUE(x$args$na.omit))) {

          write.object <- write.object[order(factor(write.object[, 1L], levels = c(levels(x$data[, 1L]), "NA"), labels = c(levels(x$data[, 1L]), "NA")),
                                             write.object[, 2L]), ]

          # Sort without NA
        } else {

          write.object <- write.object[order(factor(write.object[, 1L], levels = levels(x$data[, 1L]), labels = levels(x$data[, 1L])),
                                             write.object[, 2L]), ]

        }

      # First variable is not a factor, second variable is a factor
      } else if (isTRUE(!is.factor(x$data[, 1L]) && is.factor(x$data[, 2L]))) {

        # Sort with NA
        if (isTRUE(any(is.na(x$data)) && !isTRUE(x$args$na.omit))) {

          write.object <- write.object[order(write.object[, 1L],
                                       factor(write.object[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

        # Sort without NA
        } else {

          write.object <- write.object[order(write.object[, 1L],
                                      factor(write.object[, 2L], levels = levels(x$data[, 2L]), labels = levels(x$data[, 2L]))), ]

        }

      # First and second variable are not a factor
      } else if (isTRUE(!is.factor(x$data[, 1L]) && !is.factor(x$data[, 2L]))) {

        # Sort with NA
        if (isTRUE(any(is.na(x$data)) && !isTRUE(x$args$na.omit))) {

          write.object <- write.object[order(write.object[, 1L],
                                       factor(write.object[, 2L], levels = c(levels(x$data[, 2L]), "NA"), labels = c(levels(x$data[, 2L]), "NA"))), ]

        # Sort without NA
        } else {

          write.object <- write.object[order(write.object[, 1L], write.object[, 2L]), ]

        }

      }

      # Add column sum
      write.object <- rbind(write.object,
                            c(NA, NA, NA, colSums(write.object[write.object[, 3] == "Freq", -c(1L, 2L, 3L)])))

      write.object[nrow(write.object), 1L] <- "Total"

      #......
      # Output table not split
      if (!isTRUE(x$args$split)) {

        # Remove duplicated row labels
        write.object[, 2L] <- ifelse(duplicated(apply(write.object[, c(1L, 2L)], 1, paste, collapse = "")), NA, write.object[, 2L])
        write.object[, 1L] <- ifelse(duplicated(write.object[, 1L]), NA, write.object[, 1L])

        # Remove percentages
        if (isTRUE(x$args$print == "no")) {

          write.object <- data.frame(write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), 1L],
                                     write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), 2L],
                                     write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), -c(1L, 2L, 3L)],
                                     row.names = NULL, check.rows = FALSE,
                                     check.names = FALSE, fix.empty.names = FALSE)

        } else {

          if (isTRUE(!"row" %in% x$args$print)) {

            write.object <- write.object[-which(write.object[, 3L] == "Row %"), ]

          }

          if (isTRUE(!"col" %in% x$args$print)) {

            write.object <- write.object[-which(write.object[, 3L] == "Col %"), ]

          }

          if (isTRUE(!"total" %in% x$args$print)) {

            write.object <- write.object[-which(write.object[, 3L] == "Tot %"), ]

          }

        }

        # Add variable names
        write.object <- data.frame(c(colnames(x$data)[1L], rep(NA, times = nrow(write.object) - 1L)),
                                   write.object,
                                   row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

        names(write.object)[c(2L, 3L)] <- colnames(x$data)[c(2L, 3L)]

      #......
      # Output table split
      } else {

        # Frequencies
        write.object.abs <- data.frame(write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), 1L],
                                       write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), 2L],
                                       write.object[write.object[, 3L] == "Freq" | is.na(write.object[, 3L]), -c(1L, 2L, 3L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Row-wise percentages
        write.object.row <- data.frame(write.object[which(write.object[, 3L] == "Row %"), 1L],
                                       write.object[which(write.object[, 3L] == "Row %"), 2L],
                                       write.object[which(write.object[, 3L] == "Row %"), -c(1L, 2L, 3L, ncol(write.object))],
                                       Total = rowSums(write.object[which(write.object[, 3L] == "Row %"), -c(1L, 2L, 3L, ncol(write.object))]),
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Column-wise percentages
        write.object.col <- data.frame(write.object[which(write.object[, 3L] == "Col %"), 1L],
                                       write.object[which(write.object[, 3L] == "Col %"), 2L],
                                       write.object[which(write.object[, 3L] == "Col %"), -c(1L, 2L, 3L, ncol(write.object))],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        # Total percentages
        write.object.tot <- data.frame(write.object[write.object[, 3L] == "Tot %", 1L],
                                       write.object[write.object[, 3L] == "Tot %", 2L],
                                       write.object[write.object[, 3L] == "Tot %", -c(1L, 2L, 3L)],
                                       row.names = NULL, check.rows = FALSE,
                                       check.names = FALSE, fix.empty.names = FALSE)

        write.object.tot[nrow(write.object.tot), ncol(write.object.tot)] <- 100L

        #.............
        # Prepare list
        write.object <- list(Absolute = write.object.abs)

        if (isTRUE("row" %in% x$args$print)) {

          write.object$"Row%" <- write.object.row

        }

        if (isTRUE("col" %in% x$args$print)) {

          write.object$"Col%" <-write.object.col

        }

        if (isTRUE("total" %in% x$args$print)) {

          write.object$"Total%" <- write.object.tot

        }

      }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive Statistics, descript() ####
  }, descript = {

    #...................
    ### No Grouping, No Split ####
    if (isTRUE(is.null(x$data$group) && is.null(x$data$split))) {

      #...............
      # Select statistical measures

      print <- match(x$args$print, names(write.object))

      # Variable names
      names(write.object) <- c("Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt")

      # One variable
      if (isTRUE(ncol(x$data$x) == 1L)) {

        # Select statistical measures
        write.object <- write.object[, print]

      # More than one variable
      } else {

        # Select statistical measures
        write.object <- write.object[, c(1, print)]

      }

    #...................
    ### Grouping, No Split ####
    } else if (isTRUE(!is.null(x$data$group) && is.null(x$data$split))) {

      #...............
      # Select statistical measures

      print <- match(x$args$print, names(write.object))

      # Variable names
      names(write.object) <- c("Group", "Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt")

      # One variable
      if (isTRUE(ncol(x$data$x) == 1L)) {

        # Select statistical measures
        write.object <- write.object[, c(1, print)]

      # More than one variable
      } else {

        # Select statistical measures
        write.object <- write.object[, c(1, 2, print)]

      }

      # Convert to numeric
      write.object$Group <- ifelse(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)",
                                             x = write.object$Group), as.numeric(write.object$Group), write.object$Group)

    #...................
    ### Split, without or with Grouping ####
    } else if (isTRUE(!is.null(x$data$split))) {

      #......
      # No grouping
      if (isTRUE(is.null(x$data$group))) {

        #...............
        # Select statistical measures

        print <- match(x$args$print, names(write.object[[1]]))

        # Variable names
        write.object <- lapply(write.object, function(y) misty::df.rename(y, from = names(y), to = c("Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt")))

        # One variable
        if (isTRUE(ncol(x$data$x) == 1L)) {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, ])

        # More than one variable
        } else {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, c(1, print)])

        }

      #......
      # Grouping
      } else {

        #...............
        # Select statistical measures

        print <- match(x$args$print, names(write.object[[1]]))

        # Variable names
        write.object <- lapply(write.object, function(y) misty::df.rename(y, from = names(y), to = c("Group", "Variable", "n", "nNA", "pNA", "M", "SE.M", "Var", "SD", "Min", "p25", "Med", "p75", "Max", "Range", "IQR", "Skew", "Kurt")))

        # One variable
        if (isTRUE(ncol(x$data$x) == 1L)) {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, c(1, print)])

        # More than one variable
        } else {

          # Select statistical measures
          write.object <- lapply(write.object, function(y) y[, c(1, 2, print)])

        }

        # Convert to numeric
        write.object <- lapply(write.object, function(y) within(y, assign("Group", ifelse(grepl("(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)",
                                                                          x = y$Group), as.numeric(y$Group), y$Group))))

      }

    }

  #...................
  ### Frequency Table, freq() ####
  }, freq = {

    #...................
    ### One variable ####
    if (isTRUE(ncol(x$data) == 1)) {

      #......................
      # Values shown in columns, variables in the rows
      if (isTRUE(x$args$val.col)) {

        # Complete data
        if (isTRUE(all(!is.na(x$data)))) {

          write.object <- data.frame(Value = c("Freq", "Perc"),
                                     write.object[-nrow(write.object), -ncol(write.object)],
                                     Total = rowSums(write.object[-nrow(write.object), -ncol(write.object)]),
                                     Missing = write.object[-nrow(write.object), ncol(write.object)],
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

        # Missing data
        } else {

          write.object <- data.frame(Value = c("Freq", "Perc", "Valid Perc"),
                                     write.object[, -ncol(write.object)],
                                     Total = rowSums(write.object[, -ncol(write.object)]),
                                     Missing = write.object[, ncol(write.object)],
                                     Total = rowSums(write.object),
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

        }

      #......................
      # Values shown in rows, variables in the columns
      } else {

        # Complete data
        if (isTRUE(all(!is.na(x$data)))) {

          write.object <- data.frame(c("Value", rep("", times = nrow(write.object) - 2), "Total", "Missing"),
                                     c(write.object[, "Value"], NA),
                                     Freq = c(write.object[1:nrow(write.object) - 1, "Freq"],
                                              sum(write.object[1:nrow(write.object) - 1, "Freq"]),
                                              write.object[nrow(write.object), "Freq"]),
                                     Perc = c(write.object[1:nrow(write.object) - 1, "Perc"],
                                              sum(write.object[1:nrow(write.object) - 1, "Perc"]),
                                              write.object[nrow(write.object), "Perc"]),
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

          colnames(write.object) <- c("", "", "Freq", "Perc")

        # Missing data
        } else {

          write.object <- data.frame(c("Value", rep("", times = nrow(write.object) - 2), "Total", "Missing", "Total"),
                                     c(write.object[, "Value"], NA, NA),
                                     Freq = c(write.object[1:nrow(write.object) - 1, "Freq"],
                                              sum(write.object[1:nrow(write.object) - 1, "Freq"]),
                                              write.object[nrow(write.object), "Freq"],
                                              sum(write.object[, "Freq"])),
                                     Perc = c(write.object[1:nrow(write.object) - 1, "Perc"],
                                              sum(write.object[1:nrow(write.object) - 1, "Perc"]),
                                              write.object[nrow(write.object), "Perc"],
                                              sum(write.object[, "Perc"])),
                                     V.Perc = c(write.object[1:nrow(write.object) - 1, "V.Perc"],
                                                sum(write.object[1:nrow(write.object) - 1, "V.Perc"]), NA, NA),
                                     fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

          colnames(write.object) <- c("", "", "Freq", "Perc", "Valid Perc")

        }

      }

    #...................
    ### More than one variable ####
    } else {

      #......................
      # Variables split to multiple Excel sheets
      if (isTRUE(x$args$split)) {

        write.object <- lapply(write.object, function(y) {

          #......................
          # Values shown in columns, variables in the rows
          if (isTRUE(x$args$val.col)) {

            # Complete data
            if (isTRUE(y[1, ncol(y)] == 0)) {

              data.frame(Value = c("Freq", "Perc"),
                         y[-nrow(y), -ncol(y)], Total = rowSums(y[-nrow(y), -ncol(y)]),
                         Missing = y[-nrow(y), ncol(y)],
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            } else {

              data.frame(Value = c("Freq", "Perc", "Valid Perc"),
                         y[, -ncol(y)],
                         Total = rowSums(y[, -ncol(y)]),
                         Missing = y[, ncol(y)],
                         Total = rowSums(y),
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            }

          #......................
          # Values shown in rows, variables in the columns
          } else {

            # Complete data
            if (isTRUE(y[nrow(y), "Freq"] == 0)) {

              data.frame(c("Value", rep("", times = nrow(y) - 2), "Total", "Missing"),
                         c(y[, "Value"], NA),
                         Freq = c(y[1:nrow(y) - 1, "Freq"], sum(y[1:nrow(y) - 1, "Freq"]), y[nrow(y), "Freq"]),
                         Perc = c(y[1:nrow(y) - 1, "Perc"], sum(y[1:nrow(y) - 1, "Perc"]), y[nrow(y), "Perc"]),
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            } else {

              data.frame(c("Value", rep("", times = nrow(y) - 2), "Total", "Missing", "Total"),
                         c(y[, "Value"], NA, NA),
                         Freq = c(y[1:nrow(y) - 1, "Freq"], sum(y[1:nrow(y) - 1, "Freq"]),
                                  y[nrow(y), "Freq"],
                                  sum(y[, "Freq"])),
                         Perc = c(y[1:nrow(y) - 1, "Perc"], sum(y[1:nrow(y) - 1, "Perc"]), y[nrow(y), "Perc"], sum(y[, "Perc"])),
                         V.Perc = c(y[1:nrow(y) - 1, "V.Perc"], sum(y[1:nrow(y) - 1, "V.Perc"]), NA, NA),
                         fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            }

          }

        })

      #......................
      # Variables not split to multiple Excel sheets
      } else {

        #......................
        # Values shown in columns, variables in the rows
        if (isTRUE(x$args$val.col)) {

          # Complete data
          if (isTRUE(all(!is.na(x$data)))) {

            write.object$freq <- data.frame(write.object$freq[, "Var"],
                                            write.object$freq[, -c(1, ncol(write.object$freq))],
                                            Total = rowSums(write.object$freq[, -c(1, ncol(write.object$freq))]),
                                            Missing = write.object$freq[, ncol(write.object$freq)],
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(write.object$perc[, "Var"],
                                            write.object$perc[, -c(1, ncol(write.object$perc))],
                                            Total = rowSums(write.object$perc[, -c(1, ncol(write.object$perc))]),
                                            Missing = write.object$perc[, ncol(write.object$perc)],
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- NULL
            names(write.object) <- c("Freq", "Perc")

          # Missing data
          } else {

            write.object$freq <- data.frame(write.object$freq[, "Var"],
                                            write.object$freq[, -c(1, ncol(write.object$freq))],
                                            Total = rowSums(write.object$freq[, -c(1, ncol(write.object$freq))]),
                                            Missing = write.object$freq[, ncol(write.object$freq)],
                                            Total = rowSums(write.object$freq[, -1]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(write.object$perc[, "Var"],
                                            write.object$perc[, -c(1, ncol(write.object$perc))],
                                            Total = rowSums(write.object$perc[, -c(1, ncol(write.object$perc))]),
                                            Missing = write.object$perc[, ncol(write.object$perc)],
                                            Total = rowSums(write.object$perc[, -1]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- data.frame(write.object$v.perc[, "Var"],
                                              write.object$v.perc[, -c(1, ncol(write.object$v.perc))],
                                              Total = rowSums(write.object$v.perc[, -c(1, ncol(write.object$v.perc))]),
                                              Missing = write.object$v.perc[, ncol(write.object$v.perc)],
                                              Total = rowSums(write.object$v.perc[, -1]),
                                              fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            names(write.object) <- c("Freq", "Perc", "Valid Perc")

          }

        #......................
        # Values shown in rows, variables in the columns
        } else {

          # Complete data
          if (isTRUE(all(!is.na(x$data)))) {

            write.object$freq <- data.frame(c("Value", rep("", times = nrow(write.object$freq) - 2), "Total", "Missing"),
                                            c(write.object$freq[, "Value"], NA),
                                            rbind(write.object$freq[1:nrow(write.object$freq) - 1, -1],
                                                  colSums(write.object$freq[1:nrow(write.object$freq) - 1, -1]),
                                                  write.object$freq[nrow(write.object$freq), -1]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(c("Value", rep("", times = nrow(write.object$perc) - 2), "Total", "Missing"),
                                            c(write.object$perc[, "Value"], NA),
                                            rbind(write.object$perc[1:nrow(write.object$perc) - 1, -1],
                                                  colSums(write.object$perc[1:nrow(write.object$perc) - 1, -1]),
                                                  write.object$perc[nrow(write.object$perc), -1]),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- NULL
            names(write.object) <- c("Freq", "Perc")

          # Missing data
          } else {

            write.object$freq <- data.frame(c("Value", rep("", times = nrow(write.object$freq) - 2), "Total", "Missing", "Total"),
                                            c(write.object$freq[, "Value"], NA, NA),
                                            rbind(write.object$freq[1:nrow(write.object$freq) - 1, -1],
                                                  colSums(write.object$freq[1:nrow(write.object$freq) - 1, -1]),
                                                  write.object$freq[nrow(write.object$freq), -1], colSums(write.object$freq[, -1])),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$perc <- data.frame(c("Value", rep("", times = nrow(write.object$perc) - 2), "Total", "Missing", "Total"),
                                            c(write.object$perc[, "Value"], NA, NA),
                                            rbind(write.object$perc[1:nrow(write.object$perc) - 1, -1],
                                                  colSums(write.object$perc[1:nrow(write.object$perc) - 1, -1]),
                                                  write.object$perc[nrow(write.object$perc), -1], colSums(write.object$perc[, -1])),
                                            fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            write.object$v.perc <- data.frame(c("Value", rep("", times = nrow(write.object$v.perc) - 1), "Total"),
                                              c(write.object$v.perc[, "Value"], NA),
                                              rbind(write.object$v.perc[1:nrow(write.object$v.perc), -1],
                                                    colSums(write.object$v.perc[1:nrow(write.object$v.perc), -1])),
                                              fix.empty.names = FALSE, check.names = FALSE, row.names = NULL)

            names(write.object) <- c("Freq", "Perc", "Valid Perc")

          }

        }

      }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Coefficient Alpha and Item Statistics, item.alpha() ####
  }, item.alpha = {

    if (is.null(write.object$itemstat)) {

      write.object <- write.object$alpha
      names(write.object) <- c("Items", "Alpha")

    } else {

      names(write.object)  <- c("Alpha", "Itemstat")

      names(write.object$Alpha) <- c("n", "Items", "Alpha", "Low", "Upp")
      names(write.object$Itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "It.Cor", "Alpha")

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Confirmatory Factor Analysis, item.cfa() ####
  }, item.cfa = {

    #...................
    ### lavaan summary ####

    # Column names
    colnames(write.object$summary) <- c(write.object$summary[1, 1], "", "")

    summary <- write.object$summary[-1, ]

    #...................
    ### Covariance coverage ####

    # Add variable names in the rows
    coverage <- data.frame(colnames(write.object$coverage), write.object$coverage,
                           row.names = NULL, check.rows = FALSE,
                           check.names = FALSE, fix.empty.names = FALSE)

    #...................
    ### Univariate Sample Statistics ####

    itemstat <- write.object$itemstat

    colnames(itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Skew", "Kurt")

    #...................
    ### Univariate Counts for Ordered Variables ####

    itemfreq <- write.object$itemfreq$freq

    colnames(itemfreq)[1] <- "Variable"

    #...................
    ### Model fit ####

    fit <- write.object$fit

    colnames(fit) <- c("", "Standard", "Ad hoc", "Robust")

    #...................
    ### Parameter estimates ####

    param <- write.object$param[, -c(2, 3)]

    colnames(param) <- c("Param1", "Param2", "Estimate", "SE", "z", "pvalue", "StdYX")

    #...................
    ### Modification indices ####

    modind <- write.object$modind

    colnames(modind) <- c("lhs", "op", "rhs", "MI", "EPC", "STDYX EPC")

    #...................
    ### Write object ####

    write.object <- list(summary = summary, coverage = coverage, itemstat = itemstat,
                         itemfreq = itemfreq, fit = fit, param = param, modind = modind)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Coefficient Omega, Hierarchical Omega, and Categorical Omega, item.omega() ####
  }, item.omega = {

    if (is.null(write.object$itemstat)) {

      write.object <- write.object$omega
      names(write.object) <- c("Items", "Omega")

    } else {

      names(write.object)  <- c("Omega", "Itemstat")

      names(write.object$Omega) <- c("n", "Items", "Omega", "Low", "Upp")
      names(write.object$Itemstat) <- c("Variable", "n", "nNA", "pNA", "M", "SD", "Min", "Max", "Std.Ld", "Omega")

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Within-Group and Between-Group Correlation Matrix, multilevel.cor() ####
  }, multilevel.cor = {

    # Split results
    if (isTRUE(x$args$split)) {

      write.object <- list(summary = write.object$summary,
                           with.cor = write.object$with.cor, with.se = write.object$with.se,
                           with.stat = write.object$with.stat, with.p = write.object$with.p,
                           betw.cor = write.object$betw.cor, betw.se = write.object$betw.se,
                           betw.stat = write.object$betw.stat, betw.p = write.object$betw.p)

    # Combined results
    } else {

      write.object <- list(summary = write.object$summary,
                           cor = write.object$wb.cor, se = write.object$wb.se,
                           stat = write.object$wb.stat, p = write.object$wb.p)

    }

    # Add variable names in the rows
    write.object[-1L] <- lapply(write.object[-1L], function(y) data.frame(colnames(y), y,
                                                                          row.names = NULL, check.rows = FALSE,
                                                                          check.names = FALSE, fix.empty.names = FALSE))

    # Add 'Lower triangular: Within-Group, Upper triangular: Between-Group
    if (isTRUE(!x$args$split)) {

      write.object$summary <- data.frame(rbind(write.object$summary,
                                               c(NA, NA, NA),
                                               c("Lower triangular: Within-Group, Upper triangular: Between-Group", NA, NA)),
                                         row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Multilevel Descriptive Statistics, multilevel.descript() ####
  }, multilevel.descript = {

    write.object <- data.frame(cbind(c("No. of cases", "No. of missing values", "",
                                       "No. of clusters", "Average cluster size", "SD cluster size", "Min cluster size", "Max cluster size", "",
                                       "Mean", "Variance Within", "Variance Between", "ICC(1)", "ICC(2)", "",
                                       "Design effect", "Design effect sqrt", "Effective sample size"),
                                     rbind(x$result$no.obs, x$result$no.miss, "",
                                           x$result$no.cluster, x$result$m.cluster.size, x$result$sd.cluster.size,
                                           x$result$min.cluster.size, x$result$max.cluster.size, "",
                                           x$result$mean.x, x$result$var.w, x$result$var.b, x$result$icc1, x$result$icc2, "",
                                           x$result$deff, x$result$deff.sqrt, x$result$n.effect)),
                               stringsAsFactors = FALSE)

    #...................
    ### One variable ####
    if (isTRUE(length(x$result$no.obs) == 1L)) {

      write.object[, -1] <- as.numeric(write.object[, -1])
      names(write.object)[1] <- ""

    #...................
    ### More than one variable ####
    } else {

      write.object[, -1] <- vapply(write.object[, -1], as.numeric, FUN.VALUE = numeric(18))

      names(write.object) <- c("", names(x$result$no.obs))

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variance-Covariance Coverage, na.coverage() ####
  }, na.coverage = {

    # Add variable names in the rows
    write.object <- data.frame(colnames(write.object), write.object,
                               row.names = NULL, check.rows = FALSE,
                               check.names = FALSE, fix.empty.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Descriptive Statistics for Missing Data, na.descript() ####
  }, na.descript = {

    write.object <- data.frame(c("No. of cases", "No. of complete cases", "No. of incomplete cases", NA,
                                 "No. Of values", "No. Of observed values", "No of missing values", NA,
                                 "No. Of variables", "No. Of missing values across all variables",
                                 "   Mean", "   SD", "   Minimum", "   P25", "   P75", "   Maximum"),
                               Freq = c(write.object$no.cases, write.object$no.complete, write.object$no.incomplete, NA,
                                        write.object$no.values, write.object$no.observed.values, write.object$no.missing.values, NA,
                                        write.object$no.var, NA,
                                        write.object$no.missing.mean, write.object$no.missing.sd, write.object$no.missing.min,
                                        write.object$no.missing.p25, write.object$no.missing.p75, write.object$no.missing.max),
                               Perc = c(NA, write.object$perc.complete, write.object$perc.incomplete, NA,
                                        NA, write.object$perc.observed.values, write.object$perc.missing.values, NA,
                                        NA, NA,
                                        write.object$perc.missing.mean, write.object$perc.missing.sd, write.object$perc.missing.min,
                                        write.object$perc.missing.p25, write.object$perc.missing.p75, write.object$perc.missing.max),
                               row.names = NULL, check.rows = FALSE,
                               check.names = FALSE, fix.empty.names = FALSE)

    # Frequency table fir each variable
    write.object <- list(Summary = write.object, Table = x$result$table.miss)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Missing Data Pattern, na.pattern() ####
  }, na.pattern = {

    names(write.object)[c(1, 3)] <- c("Pattern", "Perc")

  })

  #_____________________________________________________________________________
  #
  # Write Excel file -----------------------------------------------------------

  misty::write.xlsx(write.object, file = file)

  return(invisible(write.object))

}
