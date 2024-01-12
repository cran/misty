#' Cross Tabulation
#'
#' This function creates a two-way and three-way cross tabulation with absolute
#' frequencies and row-wise, column-wise and total percentages.
#'
#' @param ...     a matrix or data frame with two or three columns. Alternatively,
#'                an expression indicating the variable names in \code{data} e.g.,
#'                \code{crosstab(x1, x2, x3, data = dat)}. Note that the operators
#'                \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                and \code{!} can also be used to select variables, see 'Details'
#'                in the \code{\link{df.subset}} function.
#' @param data    a data frame when specifying one or more variables in the
#'                argument \code{...}. Note that the argument is \code{NULL}
#'                when specifying a matrix or data frame for the argument
#'                \code{...}.
#' @param print   a character string or character vector indicating which
#'                percentage(s) to be printed on the console, i.e., no percentages
#'                (\code{"no"}) (default), all percentages (\code{"all"}),
#'                row-wise percentages (\code{"row"}), column-wise percentages
#'                (\code{"col"}), and total percentages (\code{"total"}).
#' @param freq    logical: if \code{TRUE} (default), absolute frequencies will be included
#'                in the cross tabulation.
#' @param split   logical: if \code{TRUE}, output table is split in absolute
#'                frequencies and percentage(s).
#' @param na.omit logical: if \code{TRUE} (default), incomplete cases are removed before
#'                conducting the analysis (i.e., listwise deletion).
#' @param digits  an integer indicating the number of decimal places digits
#'                to be used for displaying percentages.
#' @param as.na   a numeric vector indicating user-defined missing values,
#'                i.e. these values are converted to \code{NA} before conducting
#'                the analysis.
#' @param write   a character string naming a file for writing the output into
#'                either a text file with file extension \code{".txt"} (e.g.,
#'                \code{"Output.txt"}) or Excel file with file extention
#'                \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                name does not contain any file extension, an Excel file will
#'                be written.
#' @param append  logical: if \code{TRUE} (default), output will be appended
#'                to an existing text file with extension \code{.txt} specified
#'                in \code{write}, if \code{FALSE} existing text file will be
#'                overwritten.
#' @param check   logical: if \code{TRUE} (default), argument specification is checked.
#' @param output  logical: if \code{TRUE} (default), output is printed on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' \code{\link{freq}}, \code{\link{descript}}, \code{\link{multilevel.descript}},
#' \code{\link{na.descript}}, \code{\link{write.result}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab matrix or data frame specified in \code{...} \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list with result tables \cr
#' }
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Two-Dimensional Table
#'
#' # Example 1a: Cross Tabulation for 'vs' and 'am'
#' crosstab(mtcars[, c("vs", "am")])
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' crosstab(vs, am, data = mtcars)
#'
#' # Example 2: Cross Tabulation, print all percentages
#' crosstab(mtcars[, c("vs", "am")], print = "all")
#'
#' # Example 3: Cross Tabulation, print row-wise percentages
#' crosstab(mtcars[, c("vs", "am")], print = "row")
#'
#' # Example 4: Cross Tabulation, print col-wise percentages
#' crosstab(mtcars[, c("vs", "am")], print = "col")
#'
#' # Example 5: Cross Tabulation, print total percentages
#' crosstab(mtcars[, c("vs", "am")], print = "total")
#'
#' # Example 6: Cross Tabulation, print all percentages, split output table
#' crosstab(mtcars[, c("vs", "am")], print = "all", split = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Three-Dimensional Table
#'
#' # Example 7a: Cross Tabulation for 'vs', 'am', ane 'gear'
#' crosstab(mtcars[, c("vs", "am", "gear")])
#'
#' # Example 7b: Alternative specification using the 'data' argument
#' crosstab(vs:gear, data = mtcars)
#'
#' # Example 8: Cross Tabulation, print all percentages
#' crosstab(mtcars[, c("vs", "am", "gear")], print = "all")
#'
#' # Example 9: Cross Tabulation, print all percentages, split output table
#' crosstab(mtcars[, c("vs", "am", "gear")], print = "all", split = TRUE)
#'
#' \dontrun{
#' # Example 10a: Write Results into a text file
#' crosstab(mtcars[, c("vs", "am")], print = "all", write = "Crosstab.txt")
#'
#' # Example 10b: Write Results into a Excel file
#' crosstab(mtcars[, c("vs", "am")], print = "all", write = "Crosstab.xlsx")
#'
#' result <- crosstab(mtcars[, c("vs", "am")], print = "all", output = FALSE)
#' write.result(result, "Crosstab.xlsx")
#' }
crosstab <- function(..., data = NULL, print = c("no", "all", "row", "col", "total"),
                     freq = TRUE, split = FALSE, na.omit = TRUE, digits = 2,
                     as.na = NULL, write = NULL, append = TRUE, check = TRUE,
                     output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'data' is data frame
  if (isTRUE(!is.null(data) && !is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a matrix or data frame")

    # Extract data
    x <- data[, var.names]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data frame ####

  x.crosstab <- as.data.frame(x, stringsAsFactors = FALSE)

  # Number of variables
  x.ncol <- ncol(x)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x.crosstab <- .as.na(x.crosstab, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'x'
    if (isTRUE(ncol(x.crosstab) > 3L || ncol(x.crosstab) < 2L)) { stop("Please specify a matrix or data frame with two or three columns.", call. = FALSE) }

    # Check input 'x'
    x.zero.var <- vapply(x.crosstab, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))
    if (isTRUE(any(x.zero.var))) {

      stop(paste0("Following variables have only one unique value: ", paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

    # Check input 'print'
    if (isTRUE(any(!print %in% c("no", "all", "row", "col", "total")))) { stop("Character strings in the argument 'print' do not match with \"no\", \"all\", \"row\", \"col\" or \"total\".", call. = FALSE) }

    # Check input 'freq'
    if (isTRUE(!is.logical(freq))) { stop("Please specify TRUE or FALSE for the argument 'freq'.", call. = FALSE) }

    # Check print = "no" and freq = FALSE
    if (isTRUE(all(print == "no") && isTRUE(!freq))) { stop("Please include either percentages (i.e., print != 'no') or absolute frequencies (i.e., freq = TRUE) in the cross tabulation.", call. = FALSE) }

    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) { stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { warning("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  # Argument print
  if (isTRUE(all(c("no", "all", "row", "col", "total") %in% print))) { print <- "no" }

  if (isTRUE(length(print) == 1 && print == "all")) { print <- c("row", "col", "total") }

  # Global variable
  addmargins <- NULL

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two variables ####

  if (isTRUE(x.ncol == 2L)) {

    # If na.omit = FALSE, then include NA if any present
    if (isTRUE(!na.omit)) {

      x.crosstab <- data.frame(lapply(x.crosstab, function(y) misty::rec(y, spec = "NA = 'NA'")), stringsAsFactors = FALSE)

    } else {

      if (isTRUE(any(is.na(x.crosstab)))) {

        warning(paste0("Listwise deletion of incomplete cases, number of cases removed from the analysis: ", length(attributes(na.omit(x.crosstab))$na.action)), call. = FALSE)

      }

    }

    #...................
    ### Absolute frequencies without margins ####
    freq.a <- table(x.crosstab)

    #...................
    ### Row-wise percentages ####

    perc.r <- addmargins(prop.table(freq.a, margin = 1L) * 100L)
    perc.r[row.names(perc.r) == "Sum", ] <- addmargins(prop.table(table(x.crosstab[, 2L])) * 100L)

    rownames(perc.r)[nrow(perc.r)] <- "Total"
    colnames(perc.r)[ncol(perc.r)] <- "Total"

    #...................
    ### Column-wise percentages ####

    perc.c <- addmargins(prop.table(freq.a, margin = 2L) * 100L)
    perc.c[, colnames(perc.c) == "Sum"] <- addmargins(prop.table(table(x.crosstab[, 1L])) * 100L)

    rownames(perc.c)[nrow(perc.c)] <- "Total"
    colnames(perc.c)[ncol(perc.c)] <- "Total"

    #...................
    ### Total percentages ####

    perc.t <- addmargins(prop.table(freq.a) * 100L)

    rownames(perc.t)[nrow(perc.t)] <- "Total"
    colnames(perc.t)[ncol(perc.t)] <- "Total"

    #...................
    ### Add margins ####
    freq.a <- addmargins(freq.a)

    rownames(freq.a)[nrow(freq.a)] <- "Total"
    colnames(freq.a)[ncol(freq.a)] <- "Total"

    #...................
    ### Table ####

    result <- data.frame(rep(names(freq.a[, 1L]), times = 4L),
                             rep(c("Freq", "Row %", "Col %", "Tot %"), each = nrow(freq.a)),
                             misty::as.na(rbind(freq.a, perc.r, perc.c, perc.t), na = "NaN", check = FALSE),
                             row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

    #...................
    ### Sort Table ####

    #### First variable is a factor ####
    if (isTRUE(is.factor(x.crosstab[, 1L]))) {

      # Sort with NA
      if (isTRUE(any(is.na(x.crosstab)) && isTRUE(!na.omit))) {

        result <- result[order(factor(result[, 1L], levels = c(levels(x.crosstab[, 1L]), "NA"), labels = c(levels(x.crosstab[, 1L]), "NA"))), ]

      # Sort without NA
      } else {

        result <- result[order(factor(result[, 1L], levels = levels(x.crosstab[, 1L]), labels = levels(x.crosstab[, 1L]))), ]

      }

    #### First variable is not a factor ####
    } else {

      result.temp <- result[-which(result[, 1L] == "Total"), ]

      #### Numeric
      if (isTRUE(is.numeric(x[, 1L]))) {

        # 'NA' character
        if (isTRUE(any(x.crosstab[, 1L] == "NA", na.rm = TRUE))) {

          result.temp.NA <- result.temp[-which(result.temp[, 1L] == "NA"), ]

          result <- rbind(result.temp.NA[order(as.numeric(result.temp.NA[, 1L])), ], result.temp[which(result.temp[, 1L] == "NA"), ], result[which(result[, 1L] == "Total"), ])

        # No 'NA' character
        } else {

          result <- rbind(result.temp[order(as.numeric(result.temp[, 1L])), ], result[which(result[, 1L] == "Total"), ])

        }

      ####  Character
      } else {

        # 'NA' character
        if (isTRUE(any(x.crosstab[, 1L] == "NA", na.rm = TRUE))) {

          result.temp.NA <- result.temp[-which(result.temp[, 1L] == "NA"), ]

          result <- rbind(result.temp.NA[order(result.temp.NA[, 1L]), ], result.temp[which(result.temp[, 1L] == "NA"), ], result[which(result[, 1L] == "Total"), ])

        # No 'NA' character
        } else {

          result <- rbind(result.temp[order(result.temp[, 1L]), ], result[which(result[, 1L] == "Total"), ])

        }

      }

    }

    rownames(result) <- NULL

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Three variables ####
  } else if (isTRUE(x.ncol == 3L)) {

    # If na.omit = FALSE, then include NA if any present
    if (isTRUE(!na.omit)) {

      x.crosstab <- data.frame(lapply(x.crosstab, function(y) misty::rec(y, spec = "NA = 'NA'")), stringsAsFactors = FALSE)

    } else {

      if (isTRUE(any(is.na(x.crosstab)))) {

        warning(paste0("Listwise deletion of incomplete cases, number of cases removed from the analysis: ", length(attributes(na.omit(x.crosstab))$na.action)), call. = FALSE)

      }

    }

    #...................
    ### Absolute frequencies without margins ####

    x.table <- table(x.crosstab[, names(x.crosstab)[c(2L, 3L, 1L)]])

    freq.a <- list()
    for (i in seq_len(dim(x.table)[3L])) { freq.a[[i]] <- x.table[, , i] }

    names(freq.a) <- dimnames(x.table)[[3L]]

    #...................
    ### Row-wise percentages ####

    perc.r <- lapply(freq.a, function(y) prop.table(y, margin = 1L) * 100L)

    #...................
    ### Column-wise percentages ####

    perc.c <- lapply(freq.a, function(y) prop.table(y, margin = 2L) * 100L)

    #...................
    ### Total percentages ####

    perc.t <- lapply(freq.a, function(y) prop.table(y) * 100L)

    #...................
    ### Absolute frequencies a add margins ####

    freq.a <- lapply(freq.a, function(y) rbind(y, colSums(y)))
    freq.a <- lapply(freq.a, function(y) cbind(y, rowSums(y)))

    for (i in names(freq.a)) {

      rownames(freq.a[[i]]) <- c(rownames(freq.a[[i]])[-length(rownames(freq.a[[i]]))], "Total")
      colnames(freq.a[[i]]) <- c(colnames(freq.a[[i]])[-length(colnames(freq.a[[i]]))], "Total")

    }

    #...................
    ### Row-wise percentages a add margins ####

    for (i in names(perc.r)) {

      perc.r[[i]] <- rbind(perc.r[[i]], prop.table(freq.a[[i]][nrow(freq.a[[i]]), -ncol(freq.a[[i]])]) * 100L)
      perc.r[[i]] <- cbind(perc.r[[i]], rowSums(perc.r[[i]]))

      rownames(perc.r[[i]]) <- c(rownames(perc.r[[i]])[-length(rownames(perc.r[[i]]))], "Total")
      colnames(perc.r[[i]]) <- c(colnames(perc.r[[i]])[-length(colnames(perc.r[[i]]))], "Total")

    }

    #...................
    ### Column-wise percentages a add margins ####

    for (i in names(perc.c)) {

      perc.c[[i]] <- cbind(perc.c[[i]], prop.table(freq.a[[i]][-nrow(freq.a[[i]]), ncol(freq.a[[i]])]) * 100L)
      perc.c[[i]] <- rbind(perc.c[[i]], colSums(perc.c[[i]]))

      rownames(perc.c[[i]]) <- c(rownames(perc.c[[i]])[-length(rownames(perc.c[[i]]))], "Total")
      colnames(perc.c[[i]]) <- c(colnames(perc.c[[i]])[-length(colnames(perc.c[[i]]))], "Total")

    }

    #...................
    ### Total percentages add margins ####

    for (i in names(perc.t)) {

      perc.t[[i]] <- cbind(perc.t[[i]], prop.table(freq.a[[i]][-nrow(freq.a[[i]]), ncol(freq.a[[i]])]) * 100L)
      perc.t[[i]] <- rbind(perc.t[[i]], c(prop.table(freq.a[[i]][nrow(freq.a[[i]]), -ncol(freq.a[[i]])]) * 100L, 100L))

      rownames(perc.t[[i]]) <- c(rownames(perc.t[[i]])[-length(rownames(perc.t[[i]]))], "Total")
      colnames(perc.t[[i]]) <- c(colnames(perc.t[[i]])[-length(colnames(perc.t[[i]]))], "Total")

    }

    #...................
    ### Merge lists ####

    # Absolute frequencies
    freq.a.merge <- NULL
    for (i in seq_len(length(freq.a))) { freq.a.merge <- rbind(freq.a.merge, freq.a[[i]]) }

    # Row %
    perc.r.merge <- NULL
    for (i in seq_len(length(perc.r))) { perc.r.merge <- rbind(perc.r.merge, perc.r[[i]]) }

    # Column %
    perc.c.merge <- NULL
    for (i in seq_len(length(perc.c))) { perc.c.merge <- rbind(perc.c.merge, perc.c[[i]]) }

    # Total %
    perc.t.merge <- NULL
    for (i in seq_len(length(perc.t))) { perc.t.merge <- rbind(perc.t.merge, perc.t[[i]]) }

    #...................
    ### Result table ####

    result <- data.frame(rep(names(freq.a), each = nrow(freq.a[[1L]]), times = 4L),
                         rep(rownames(freq.a[[1L]]), times = 4L*length(freq.a)),
                         rep(c("Freq", "Row %", "Col %", "Tot %"), each = nrow(freq.a.merge)),
                         misty::as.na(rbind(freq.a.merge, perc.r.merge, perc.c.merge, perc.t.merge), na = "NaN", check = FALSE),
                         row.names = NULL, check.rows = FALSE, check.names = FALSE, fix.empty.names = FALSE)

    #...................
    ### Sort Table ####

    #### First and second variables are factors
    if (isTRUE(is.factor(x[, 1L]) && is.factor(x[, 2L]))) {

      ##### Sort with NA
      if (isTRUE(any(x.crosstab == "NA", na.rm = TRUE) && isTRUE(!na.omit))) {

        result <- result[order(factor(result[, 1L], levels = c(levels(x.crosstab[, 1L]), "NA"), labels = c(levels(x.crosstab[, 1L]), "NA")),
                               factor(result[, 2L], levels = c(levels(x.crosstab[, 2L]), "NA"), labels = c(levels(x.crosstab[, 2L]), "NA"))), ]

      ##### Sort without NA
      } else {

        result <- result[order(factor(result[, 1L], levels = levels(x.crosstab[, 1L]), labels = levels(x.crosstab[, 1L])),
                               factor(result[, 2L], levels = levels(x.crosstab[, 2L]), labels = levels(x.crosstab[, 2L]))), ]

      }

    #### First variable is a factor, second variable is not a factor
    } else if (isTRUE(is.factor(x[, 1L]) && !is.factor(x[, 2L]))) {

      ##### Sort with NA
      if (isTRUE(any(x.crosstab == "NA", na.rm = TRUE) && isTRUE(!na.omit))) {

        result.temp <- NULL
        for (i in unique(result[, 1L])) {

          temp <- result[result[, 1L] == i, ]

          temp.temp <- temp[-which(temp[, 2L] %in% c("NA", "Total")), ]

          # Second variable is numeric
          if (isTRUE(is.numeric(x[, 2L]))) {

            result.temp <- rbind(result.temp, rbind(temp.temp[order(as.numeric(temp.temp[, 2L])), ], temp[which(temp[, 2L] == "NA"), ], temp[which(temp[, 2L] == "Total"), ]))

          # Second variable is character
          } else {

            result.temp <- rbind(result.temp, rbind(temp.temp[order(temp.temp[, 2L]), ], temp[which(temp[, 2L] == "NA"), ], temp[which(temp[, 2L] == "Total"), ]))

          }

        }

        result <- result.temp

      ##### Sort without NA
      } else {

        result.temp <- NULL
        for (i in unique(result[, 1L])) {

          temp <- result[result[, 1L] == i, ]

          temp.temp <- temp[-which(temp[, 2L] == "Total"), ]

          # Second variable is numeric
          if (isTRUE(is.numeric(x[, 2L]))) {

            result.temp <- rbind(result.temp, rbind(temp.temp[order(as.numeric(temp.temp[, 2L])), ], temp[which(temp[, 2L] == "Total"), ]))

          # Second variable is character
          } else {

            result.temp <- rbind(result.temp, rbind(temp.temp[order(temp.temp[, 2L]), ], temp[which(temp[, 2L] == "Total"), ]))

          }

        }

        result <- result.temp

      }

    #### First variable is not a factor, second variable is a factor
    } else if (isTRUE(!is.factor(x[, 1L]) && is.factor(x[, 2L]))) {

      ##### Sort with NA
      if (isTRUE(any(x.crosstab == "NA", na.rm = TRUE) && isTRUE(!na.omit))) {

        # First variable is numeric
        if (isTRUE(is.numeric(x[, 1L]))) {

          result <- result[order(factor(result[, 1L], levels = c(sort(unique(as.numeric(suppressWarnings(misty::chr.omit(result[, 1L], omit = "NA"))))))),
                                 factor(result[, 2L], levels = c(levels(x.crosstab[, 2L]), "NA"), labels = c(levels(x.crosstab[, 2L]), "NA"))), ]


        # First variable is character
        } else {

          result <- result[order(factor(result[, 1L], levels = c(sort(unique(suppressWarnings(misty::chr.omit(result[, 1L], omit = "NA")))))),
                                 factor(result[, 2L], levels = c(levels(x.crosstab[, 2L]), "NA"), labels = c(levels(x.crosstab[, 2L]), "NA"))), ]

        }

      ##### Sort without NA
      } else {

        # First variable is numeric
        if (isTRUE(is.numeric(x[, 1L]))) {

          result <- result[order(factor(result[, 1L], levels = sort(unique(as.numeric(result[, 1L])))),
                                 factor(result[, 2L], levels = levels(x.crosstab[, 2L]), labels = levels(x.crosstab[, 2L]))), ]

        # First variable is character
        } else {

          result <- result[order(factor(result[, 1L], levels = sort(unique(result[, 1L]))),
                                 factor(result[, 2L], levels = levels(x.crosstab[, 2L]), labels = levels(x.crosstab[, 2L]))), ]

        }

      }

    #### First and second variables are not factors
    } else if (isTRUE(!is.factor(x[, 1L]) && !is.factor(x[, 2L]))) {

      ##### Sort with NA
      if (isTRUE(any(x.crosstab == "NA", na.rm = TRUE) && isTRUE(!na.omit))) {

        # NA in first variable
        if (isTRUE(any(result[, 1L] == "NA"))) {

          # First variable is numeric
          if (isTRUE(is.numeric(x[, 1L]))) {

            value.unique <- c(sort(as.numeric(suppressWarnings(misty::chr.omit(unique(result[, 1L]), omit = "NA", check = FALSE)))), "NA")

          # First variable is character
          } else {

            value.unique <- c(sort(suppressWarnings(misty::chr.omit(unique(result[, 1L]), omit = "NA", check = FALSE))), "NA")

          }

        # No NA in first variable
        } else {

          # First variable is numeric
          if (isTRUE(is.numeric(x[, 1L]))) {

            value.unique <- sort(as.numeric(unique(result[, 1L])))

          # First variable is character
          } else {

            value.unique <- sort(unique(result[, 1L]))

          }

        }

        result.temp <- NULL
        for (i in value.unique) {

          temp <- result[result[, 1L] == i, ]

          temp.temp <- temp[-which(temp[, 2L] %in% c("NA", "Total")), ]

          # Second variable is numeric
          if (isTRUE(is.numeric(x[, 2L]))) {

            result.temp <- rbind(result.temp, rbind(temp.temp[order(as.numeric(temp.temp[, 2L])), ], temp[which(temp[, 2L] == "NA"), ], temp[which(temp[, 2L] == "Total"), ]))

          # Second variable is chracter
          } else {

            result.temp <- rbind(result.temp, rbind(temp.temp[order(temp.temp[, 2L]), ], temp[which(temp[, 2L] == "NA"), ], temp[which(temp[, 2L] == "Total"), ]))

          }

        }

        result <- result.temp

      ##### Sort without NA
      } else {

        result.temp <- NULL
        for (i in unique(result[, 1L])) {

          temp <- result[result[, 1L] == i, ]

          temp.temp <- temp[-which(temp[, 2L] == "Total"), ]

          # Second variable is numeric
          if (isTRUE(is.numeric(x[, 2L]))) {

            result.temp <- rbind(result.temp, rbind(temp.temp[order(as.numeric(temp.temp[, 2L])), ],  temp[which(temp[, 2L] == "Total"), ]))

          # Second variable is character
          } else {

            result.temp <- rbind(result.temp, rbind(temp.temp[order(temp.temp[, 2L]), ],  temp[which(temp[, 2L] == "Total"), ]))

          }

        }

        result <- result.temp

      }

    }

    #...................
    ### Add results for total ####

    result <- rbind(result,
                    setNames(data.frame(cbind("Total", suppressWarnings(crosstab(x[, c(2L, 3L)], as.na = as.na, na.omit = na.omit, check = FALSE, output = FALSE))$result$crosstab)), nm = colnames(result)))

    rownames(result) <- NULL

    #...................
    ### Return objects ####

    freq.a <- freq.a.merge
    perc.r <- perc.r.merge
    perc.c <- perc.c.merge
    perc.t <- perc.t.merge

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "crosstab",
                 data = x.crosstab,
                 args = list(freq = freq, print = print, split = split, na.omit = na.omit,
                             digits = digits, as.na = as.na, write = write, append = append,
                             check = check, output = output),
                 result = list(crosstab = result, freq.a = freq.a, perc.r = perc.r, perc.c = perc.c, perc.t = perc.t))

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # write Result ---------------------------------------------------------------

  if (isTRUE(!is.null(write))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Text file ####

    if (isTRUE(grepl("\\.txt", write))) {

      # Send R output to textfile
      sink(file = write, append = ifelse(isTRUE(file.exists(write)), append, FALSE), type = "output", split = FALSE)

      if (append && isTRUE(file.exists(write))) { write("", file = write, append = TRUE) }

      # Print object
      print(object, check = FALSE)

      # Close file connection
      sink()

      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      ## Excel file ####

    } else {

      misty::write.result(object, file = write)

    }

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
