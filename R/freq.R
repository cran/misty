#' Frequency Tables
#'
#' This function computes frequency tables with absolute and percentage frequencies for one or more than one variable.
#'
#' By default, the function displays the absolute and percentage frequencies when specifying one variable in the
#' argument \code{x}, while the function displays only the absolute frequencies when more than one variable is specified.
#' The function displays valid percentage frequencies only in the presence of missing values and excludes variables
#' with all values missing from the analysis. Note that it is  possible to mix numeric variables, factors, and character
#' variables in the data frame specified in the argument \code{x}.
#'
#' @param x           a vector, factor, matrix or data frame.
#' @param print       a character string indicating which percentage(s) to be printed on the console, i.e.,
#'                    no percentages (\code{"no"}), all percentages (\code{"all"}), percentage frequencies
#'                    (\code{"print"}), and valid percentage frequencies (\code{"v.perc"}).
#'                    Default setting when specifying one variable in \code{x} is \code{print = "all"}, while
#'                    default setting when specifying more than one variable in \code{x} is \code{print = "no"}
#'                    unless \code{split = TRUE}.
#' @param freq        logical: if \code{TRUE} (default), absolute frequencies will be shown on the console.
#' @param split       logical: if \code{TRUE}, output table is split by variables when specifying more than one
#'                    variable in \code{x}.
#' @param labels      logical: if \code{TRUE} (default), labels for the factor levels will be used.
#' @param val.col     logical: if \code{TRUE}, values are shown in the columns, variables in the rows.
#' @param exclude     an integer value indicating the maximum number of unique values for variables to be
#'                    included in the analysis when specifying more than one variable in \code{x}, i.e.,
#'                    variables with the number of unique values exceeding \code{exclude} will be excluded
#'                    from the analysis.
#' @param digits      an integer value indicating the number of decimal places to be used for displaying
#'                    percentages.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{crosstab}}, \code{\link{descript}}, \code{\link{multilevel.descript}},
#' \code{\link{na.descript}}.
#'
#' @references
#' Becker, R. A., Chambers, J. M., & Wilks, A. R. (1988). \emph{The New S Language}. Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns an object of class \code{freq}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(3, 3, 2, 3, 2, 3, 3, 2, 1, -99),
#'                   x2 = c(2, 2, 1, 3, 1, 1, 3, 3, 2, 2),
#'                   y1 = c(1, 4, NA, 5, 2, 4, 3, 5, NA, 1),
#'                   y2 = c(2, 3, 4, 3, NA, 4, 2, 3, 4, 5),
#'                   z = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#'
#' # Frequency table for one variable
#' freq(dat$x1)
#'
#' # Frequency table for one variable,
#' # values shown in columns
#' freq(dat$x1, val.col = TRUE)
#'
#' # Frequency table for one variable,
#' # convert value -99 into NA
#' freq(dat$x1, as.na = -99)
#'
#' # Frequency table for one variable
#' # use 3 digit for displaying percentages
#' freq(dat$x1, digits = 3)
#'
#' # Frequency table for more than one variable
#' freq(dat[, c("x1", "x2", "y1", "y2")])
#'
#' # Frequency table for more than one variable,
#' # values shown in columns
#' freq(dat[, c("x1", "x2", "y1", "y2")], val.col = TRUE)
#'
#' # Frequency table for more than one variable,
#' # with percentage frequencies
#' freq(dat[, c("x1", "x2", "y1", "y2")], print = "all")
#'
#' # Frequency table for more than one variable,
#' # with percentage frequencies, values shown in columns
#' freq(dat[, c("x1", "x2", "y1", "y2")], print = "all", val.col = TRUE)
#'
#' # Frequency table for more than one variable,
#' # split output table
#' freq(dat[, c("x1", "x2", "y1", "y2")], split = TRUE)
#'
#' # Frequency table for more than one variable,
#' # exclude variables with more than 5 unique values
#' freq(dat, exclude = 5)
#'
#' # Frequency table for a factor
#' freq(factor(c("a", "a", "b", "c", "b")))
#'
#' # Frequency table for one variable,
#' # do not use labels of the factor levels
#' freq(factor(c("a", "a", "b", "c", "b")), labels = FALSE)
freq <- function(x, print = c("no", "all", "perc", "v.perc"), freq = TRUE, split = FALSE,
                 labels = TRUE, val.col = FALSE, exclude = 15, digits = 2, as.na = NULL,
                 check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a vector, factor, matrix or data frame for the argument 'x'", call. = FALSE)

  }

  #......
  # Vector, matrix or data frame for the argument 'x'?
  if (!is.vector(x) && !is.factor(x) && !is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a vector, factor, matrix or data frame for the argument 'x'", call. = FALSE)

  }

  #-----------------------------------------
  # Data.frame

  x.df <- as.data.frame(x, stringsAsFactors = FALSE)

  ####################################################################################
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x.df <- misty::as.na(x.df, na = as.na, check = check)

  }

  ####################################################################################
  # Input check

  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #..................
    # Check input 'x': All values missing
    if (all(is.na(x.df))) {

       stop("All values in the vector, matrix or data frame specified in the argument 'x' are missing.", call. = FALSE)

    }

    #..................
    # Check input 'x': Exclude variables with all missing
    if (length(x.df) > 1) {

      x.df.na <- sapply(x.df, function(y) all(is.na(y)))
      if (any(x.df.na)) {

        warning(paste0("Variables with all values missing are excluded from the analysis: ",
                       paste(names(x.df)[x.df.na], collapse = ", ")),  call. = FALSE)

        # Exclude variables with all values missing
        x.df <- x.df[, !x.df.na, drop = FALSE]
        x <- x[, !x.df.na, drop = FALSE]

      }

    }

    #..................
    # Check input 'print'
    if (any(!print %in% c("no", "all", "perc", "v.perc"))) {

      stop("Character string in the argument 'print' does not match with \"no\", \"all\", \"perc\", or \"v.perc\".",
           call. = FALSE)

    }

    #..................
    # Check input 'print'
    if (!all(c("no", "all", "perc", "v.perc") %in% print)) {

      if (length(print) != 1) {

        stop("Please specify one of the character strings \"no\", \"all\", \"perc\", or \"v.perc\" for the argument 'print'.",
                call. = FALSE)

      }

    }

    #..................
    # Check input 'freq'
    if (isFALSE(isTRUE(freq) || isFALSE(freq))) {

      stop("Please specify TRUE or FALSE for the argument 'freq'.", call. = FALSE)

    }

    #..................
    # No frequencies and percentages
    if (all(print == "no") && isFALSE(freq)) {

      stop("Please specify print = \"all\", print = \"perc\", or print = \"v.perc\" when specifying freq = TRUE.",
           call. = FALSE)

    }

    #..................
    # Check input 'split'
    if (isFALSE(isTRUE(split) || isFALSE(split))) {

      stop("Please specify TRUE or FALSE for the argument 'split'.", call. = FALSE)

    }

    #..................
    # Check input 'labels'
    if (isFALSE(isTRUE(labels) || isFALSE(labels))) {

      stop("Please specify TRUE or FALSE for the argument 'labels'.", call. = FALSE)

    }

    #..................
    # Check input 'val.col'
    if (isFALSE(isTRUE(val.col) || isFALSE(val.col))) {

      stop("Please specify TRUE or FALSE for the argument 'val.col'.", call. = FALSE)

    }

    #..................
    # Check input 'exclude'
    if (exclude %% 1 != 0 || exclude < 0) {

      stop("Specify a positive integer number for the argument 'exclude'.", call. = FALSE)

    }

    #..................
    # Check input 'digits'
    if (digits %% 1 != 0 || digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #..................
    # Check input 'output'
    if (isFALSE(isTRUE(output) || isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Argument 'print'

  #..................
  # One variable
  if (length(x.df) == 1) {

    if (all(c("no", "all", "perc", "v.perc") %in% print)) { print <- c("perc", "v.perc") }

  #..................
  # More than one variable
  } else {

    if (isTRUE(freq)) {

      if (isFALSE(split)) {

        if (all(c("no", "all", "perc", "v.perc") %in% print)) { print <- "no" }

      } else {

        if (all(c("no", "all", "perc", "v.perc") %in% print)) { print <- c("perc", "v.perc") }

      }

    } else {

      if (all(c("no", "all", "perc", "v.perc") %in% print)) { print <- c("perc", "v.perc") }

    }

  }

  if (all(print == "all")) { print <- c("perc", "v.perc") }

  #-----------------------------------------
  # Factor labels

  # Factors
  x.df.factor <- sapply(x.df, function(y) is.factor(y))

  # If at least one factor and isFALSE(labels)
  if (isTRUE(any(x.df.factor)) && isFALSE(labels)) {

    # More than one factor
    if (sum(x.df.factor) > 1) {

      # Unique factor levels for each variable
      factor.unique <- lapply(x.df[, x.df.factor, drop = FALSE], levels)

      if (isTRUE(any(apply(combn(length(x.df[, x.df.factor]), 2), 2, function(y) !identical(factor.unique[[y[1]]], factor.unique[[y[2]]]))))) {

        warning("Variable do not have the same factor levels, i.e., values may not be comparable across variables.",
                    call. = FALSE)

      }

    }

    # Remove labels
    x.df <- data.frame(lapply(x.df, function(y) if (is.factor(y)) as.numeric(y) else y), stringsAsFactors = FALSE)

  }

  #-----------------------------------------
  # Exclude variables

  if (length(x.df) > 1) {

    x.df.exclude <- which(sapply(x.df, function(y) length(unique(na.omit(y)))) > exclude)

    if (length(x.df.exclude) > 0) {

      x.df <- x.df[, -x.df.exclude]

      if (length(x.df) == 0) {

        stop(paste0("After excluding variables with more than ", exclude, " unique values, no variables are left for the analysis."),
                    call. = FALSE)

      } else {

        warning(paste0("Variables with more than ", exclude, " unique values were excluded: ", paste(names(x.df.exclude), collapse = ", ")),
                call. = FALSE)

      }

    }

  }

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # One variable

  if (length(x.df) == 1) {

    x.df.abs <- table(x.df, useNA = "always")
    x.df.perc <- prop.table(x.df.abs) * 100
    x.df.v.perc <- prop.table(table(x.df, useNA = "no")) * 100

    #..................
    # Values in columns
    if (isFALSE(val.col)) {

      freqtab <- data.frame(matrix(c(na.omit(names(x.df.abs)), "NA", as.vector(x.df.abs), as.vector(x.df.perc), as.vector(x.df.v.perc), NA), ncol = 4,
                                   dimnames = list(NULL, c("Value", "Freq", "Perc", "V.Perc"))), stringsAsFactors = FALSE)

    #..................
    # Values in rows
    } else {

      freqtab <- data.frame(matrix(c(x.df.abs, x.df.perc, x.df.v.perc, NA), nrow = 3, dimnames = list(c("Freq", "Perc", "V.Perc"), names(x.df.abs)), byrow = TRUE),
                            stringsAsFactors = FALSE, check.names = FALSE)

    }

  }

  #-----------------------------------------
  # More than one variable

  if (length(x.df) > 1) {

    #........................................
    # split = FALSE
    if (isFALSE(split)) {

      #....
      # Factors
      if (any(sapply(x.df, is.numeric)) && any(sapply(x.df, is.factor))) {

        x.df.levels <- sort(unique(c(as.character(unlist(x.df[, sapply(x.df, is.factor)])),
                                     unlist(x.df[, -which(sapply(x.df, is.factor))]))))

      } else {

        x.df.levels <- sort(unique(unlist(x.df)))

      }

      x.df.abs <- sapply(x.df, function(y) table(factor(y, levels = x.df.levels), useNA = "always"))
      x.df.perc <- apply(x.df.abs, 2, function(y) ifelse(y != 0, prop.table(y) * 100, 0))
      x.df.v.perc <- apply(sapply(x.df, function(y) table(factor(y, levels = x.df.levels), useNA = "no")), 2, function(z) prop.table(z) * 100)

      #...
      # Values in rows
      if (isFALSE(val.col)) {

        abs.freqtab <- data.frame(cbind(Value = rownames(x.df.abs), x.df.abs), stringsAsFactors = FALSE)
        perc.freqtab <- data.frame(cbind(Value = rownames(x.df.perc), x.df.perc), stringsAsFactors = FALSE)
        v.perc.freqtab <- data.frame(cbind(Value = rownames(x.df.v.perc), x.df.v.perc), stringsAsFactors = FALSE)

        row.names(abs.freqtab) <- NULL
        row.names(perc.freqtab) <- NULL
        row.names(v.perc.freqtab) <- NULL

      #...
      # Values in columns
      } else {

        abs.freqtab <- data.frame(Var = colnames(x.df.abs), matrix(t(x.df.abs), nrow = ncol(x.df.abs), dimnames = list(NULL, rownames(x.df.abs))),
                                  check.names = FALSE, stringsAsFactors = FALSE)
        perc.freqtab <- data.frame(Var = colnames(x.df.perc), matrix(t(x.df.perc), nrow = ncol(x.df.perc), dimnames = list(NULL, rownames(x.df.perc))),
                                   check.names = FALSE, stringsAsFactors = FALSE)
        v.perc.freqtab <- data.frame(Var = colnames(x.df.v.perc), matrix(t(x.df.v.perc), nrow = ncol(x.df.v.perc), dimnames = list(NULL, rownames(x.df.v.perc))),
                                     check.names = FALSE, stringsAsFactors = FALSE)

      }

      freqtab <- list(freq = abs.freqtab, perc = perc.freqtab, v.perc = v.perc.freqtab)

    #........................................
    # split = TRUE
    } else {


      freqtab <- lapply(x.df, function(y) misty::freq(y, labels = labels, val.col = val.col,
                                                      as.na = as.na, check = FALSE, output = FALSE)$result)
    }

  }

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 data = x,
                 args = list(print = print, freq = freq, split = split, labels = labels,
                             val.col = val.col, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = freqtab)

  class(object) <- "freq"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
