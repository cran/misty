#' Frequency Table
#'
#' This function computes a frequency table with absolute and percentage frequencies for one or more than one variable.
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
#' @param split       logical: if \code{TRUE}, output table is split by variables when specifying more than
#'                    one variable in \code{x}.
#' @param labels      logical: if \code{TRUE} (default), labels for the factor levels will be used.
#' @param val.col     logical: if \code{TRUE}, values are shown in the columns, variables in the rows.
#' @param exclude     an integer value indicating the maximum number of unique values for variables to be
#'                    included in the analysis when specifying more than one variable in \code{x} i.e.,
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
#' \code{\link{crosstab}}, \code{\link{descript}}, \code{\link{multilevel.descript}}, \code{\link{na.descript}}.
#'
#' @references
#' Becker, R. A., Chambers, J. M., & Wilks, A. R. (1988). \emph{The New  S Language}. Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#' function call (\code{call}), type of analysis (\code{type}),  matrix or data frame specified in
#' \code{x} (\code{data}), specification of function arguments (\code{args}), and
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
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a vector, factor, matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #-----------------------------------------
  # Data.frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #-----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

  }

  ####################################################################################
  # Input check

  # Split message
  message.split <- FALSE

  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x': All values missing
    if (isTRUE(all(is.na(x)))) {

       stop("All values in the vector, matrix or data frame specified in the argument 'x' are missing.", call. = FALSE)

    }

    #......
    # Check input 'x': Exclude variables with all missing
    if (isTRUE(length(x) > 1L)) {

      x.na <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
      if (isTRUE(any(x.na))) {

        warning(paste0("Variables with all values missing were excluded from the analysis: ",
                       paste(names(x)[x.na], collapse = ", ")), call. = FALSE)

        # Exclude variables with all values missing
        x <- x[, !x.na, drop = FALSE]

      }

    }

    #......
    # Check input 'print'
    if (isTRUE(any(!print %in% c("no", "all", "perc", "v.perc")))) {

      stop("Character string in the argument 'print' does not match with \"no\", \"all\", \"perc\", or \"v.perc\".",
           call. = FALSE)

    }

    #......
    # Check input 'print'
    if (isTRUE(!all(c("no", "all", "perc", "v.perc") %in% print))) {

      if (isTRUE(length(print) != 1L)) {

        stop("Please specify one of the character strings \"no\", \"all\", \"perc\", or \"v.perc\" for the argument 'print'.",
                call. = FALSE)

      }

    }

    #......
    # Check input 'freq'
    if (isTRUE(!is.logical(freq))) {

      stop("Please specify TRUE or FALSE for the argument 'freq'.", call. = FALSE)

    }

    #......
    # No frequencies and percentages
    if (isTRUE(all(print == "no") && !isTRUE(freq))) {

      stop("Please specify print = \"all\", print = \"perc\", or print = \"v.perc\" when specifying freq = FALSE.",
           call. = FALSE)

    }

    #......
    # Check input 'split'
    if (isTRUE(!is.logical(split))) {

      stop("Please specify TRUE or FALSE for the argument 'split'.", call. = FALSE)

    }

    if (isTRUE(split && ncol(x) == 1)) {

      split <- FALSE
      message.split <- TRUE

    }

    #......
    # Check input 'labels'
    if (isTRUE(!is.logical(labels))) {

      stop("Please specify TRUE or FALSE for the argument 'labels'.", call. = FALSE)

    }

    #......
    # Check input 'val.col'
    if (isTRUE(!is.logical(val.col))) {

      stop("Please specify TRUE or FALSE for the argument 'val.col'.", call. = FALSE)

    }

    #......
    # Check input 'exclude'
    if (isTRUE(exclude %% 1L != 0L || exclude < 0L)) {

      stop("Specify a positive integer number for the argument 'exclude'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Argument 'print'

  #..................
  # One variable
  if (isTRUE(length(x) == 1L)) {

    if (isTRUE(all(c("no", "all", "perc", "v.perc") %in% print))) { print <- c("perc", "v.perc") }

  #..................
  # More than one variable
  } else {

    if (isTRUE(freq)) {

      if (!isTRUE(split)) {

        if (isTRUE(all(c("no", "all", "perc", "v.perc") %in% print))) { print <- "no" }

      } else {

        if (isTRUE(all(c("no", "all", "perc", "v.perc") %in% print))) { print <- c("perc", "v.perc") }

      }

    } else {

      if (isTRUE(all(c("no", "all", "perc", "v.perc") %in% print))) { print <- c("perc", "v.perc") }

    }

  }

  if (isTRUE(all(print == "all"))) { print <- c("perc", "v.perc") }

  #-----------------------------------------
  # Factor labels

  # Factors
  x.factor <- vapply(x, function(y) is.factor(y), FUN.VALUE = logical(1))

  # If at least one factor and !isTRUE(labels)
  if (isTRUE(any(x.factor)) && !isTRUE(labels)) {

    # More than one factor
    if (isTRUE(sum(x.factor) > 1L)) {

      # Unique factor levels for each variable
      factor.unique <- lapply(x[, x.factor, drop = FALSE], levels)

      if (isTRUE(any(apply(combn(length(x[, x.factor]), 2L), 2, function(y) !identical(factor.unique[[y[1L]]], factor.unique[[y[2L]]]))))) {

        warning("Variable do not have the same factor levels, i.e., values may not be comparable across variables.",
                call. = FALSE)

      }

    }

    # Remove labels
    x <- data.frame(lapply(x, function(y) if (isTRUE(is.factor(y))) as.numeric(y) else y), stringsAsFactors = FALSE)

  }

  #-----------------------------------------
  # Exclude variables

  if (isTRUE(length(x) > 1 && !split)) {

    x.exclude <- which(vapply(x, function(y) length(unique(na.omit(y))), FUN.VALUE = 1L) > exclude)

    if (isTRUE(length(x.exclude) > 0L)) {

      x <- x[, -x.exclude, drop = FALSE]

      if (isTRUE(length(x) == 0L)) {

        stop(paste0("After excluding variables with more than ", exclude, " unique values, no variables are left for the analysis."),
             call. = FALSE)

      } else {

        warning(paste0("Variables with more than ", exclude, " unique values were excluded: ", paste(names(x.exclude), collapse = ", ")),
                call. = FALSE)

      }

    }

  }

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # One variable

  if (isTRUE(length(x) == 1L)) {

    x.abs <- table(x, useNA = "always")
    x.perc <- prop.table(x.abs) * 100L
    x.v.perc <- prop.table(table(x, useNA = "no")) * 100L

    #..................
    # Values in columns
    if (!isTRUE(val.col)) {

      freqtab <- data.frame(matrix(c(na.omit(names(x.abs)), "NA", as.vector(x.abs), as.vector(x.perc), as.vector(x.v.perc), NA), ncol = 4L,
                                   dimnames = list(NULL, c("Value", "Freq", "Perc", "V.Perc"))), stringsAsFactors = FALSE)

    #..................
    # Values in rows
    } else {

      freqtab <- data.frame(matrix(c(x.abs, x.perc, x.v.perc, NA), nrow = 3L, dimnames = list(c("Freq", "Perc", "V.Perc"), names(x.abs)), byrow = TRUE),
                            stringsAsFactors = FALSE, check.names = FALSE)

    }

  }

  #-----------------------------------------
  # More than one variable

  if (isTRUE(length(x) > 1L)) {

    #........................................
    # split = FALSE
    if (!isTRUE(split)) {

      #...
      # Unique levels
      x.levels <- NULL

      #...
      # Numeric variables
      if (isTRUE(any(vapply(x, is.numeric, FUN.VALUE = logical(1))))) {

        x.levels <- c(x.levels, na.omit(sort(unique(unlist(x[, vapply(x, is.numeric, FUN.VALUE = logical(1L))])))))

      }

      #...
      # Factors
      if (isTRUE(any(vapply(x, is.factor, FUN.VALUE = logical(1))))) {

        f.levels <- sort(unique(unname(unlist(sapply(x[, vapply(x, is.factor, FUN.VALUE = logical(1L))], function(y) (as.character(y)))))))

        # Factor levels are numbers
        if (isTRUE(all(unlist(strsplit(f.levels, "")) %in% 0:9))) {

          x.levels <- sort(c(x.levels, as.numeric(f.levels)))

        # Factor levels are character
        } else {

          x.levels <- c(x.levels, f.levels)

        }

      }

      #...
      # Character
      if (isTRUE(any(vapply(x, is.character, FUN.VALUE = logical(1L))))) {

        x.levels <- c(x.levels, na.omit(sort(unique(unlist(x[, vapply(x, is.character, FUN.VALUE = logical(1L))])))))

      }

      #...
      # Logical
      if (isTRUE(any(vapply(x, is.logical, FUN.VALUE = logical(1))))) {

        x.levels <- c(x.levels, c(TRUE, FALSE))

      }

      x.levels <- unique(x.levels)

      #--------------------------------------------------------

      x.abs <- vapply(x, function(y) table(factor(y, levels = x.levels), useNA = "always"), FUN.VALUE = integer(length(x.levels) + 1L))
      x.perc <- apply(x.abs, 2, function(y) ifelse(y != 0L, prop.table(y) * 100L, 0L))
      x.v.perc <- apply(vapply(x, function(y) table(factor(y, levels = x.levels), useNA = "no"), FUN.VALUE = integer(length(x.levels))), 2, function(z) prop.table(z) * 100L)

      #...
      # Values in rows
      if (!isTRUE(val.col)) {

        abs.freqtab <- data.frame(cbind(Value = rownames(x.abs), x.abs), stringsAsFactors = FALSE)
        perc.freqtab <- data.frame(cbind(Value = rownames(x.perc), x.perc), stringsAsFactors = FALSE)
        v.perc.freqtab <- data.frame(cbind(Value = rownames(x.v.perc), x.v.perc), stringsAsFactors = FALSE)

        row.names(abs.freqtab) <- NULL
        row.names(perc.freqtab) <- NULL
        row.names(v.perc.freqtab) <- NULL

      #...
      # Values in columns
      } else {

        abs.freqtab <- data.frame(Var = colnames(x.abs), matrix(t(x.abs), nrow = ncol(x.abs), dimnames = list(NULL, rownames(x.abs))),
                                  check.names = FALSE, stringsAsFactors = FALSE)
        perc.freqtab <- data.frame(Var = colnames(x.perc), matrix(t(x.perc), nrow = ncol(x.perc), dimnames = list(NULL, rownames(x.perc))),
                                   check.names = FALSE, stringsAsFactors = FALSE)
        v.perc.freqtab <- data.frame(Var = colnames(x.v.perc), matrix(t(x.v.perc), nrow = ncol(x.v.perc), dimnames = list(NULL, rownames(x.v.perc))),
                                     check.names = FALSE, stringsAsFactors = FALSE)

      }

      freqtab <- list(freq = abs.freqtab, perc = perc.freqtab, v.perc = v.perc.freqtab)

    #........................................
    # split = TRUE
    } else {


      freqtab <- lapply(x, function(y) misty::freq(y, labels = labels, val.col = val.col,
                                                      as.na = as.na, check = FALSE, output = FALSE)$result)
    }

  }

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "freq",
                 data = x,
                 args = list(print = print, freq = freq, split = split, labels = labels,
                             val.col = val.col, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = freqtab)

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  #--------------------------------------------------------
  # Message
  if (isTRUE(message.split)) {

    message("Argument 'split' set to FALSE because only one variable was specified in 'x'.")

  }

  return(invisible(object))

}
