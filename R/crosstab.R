#' Cross Tabulation
#'
#' This function creates a two-way and three-way cross tabulation with absolute frequencies and row-wise, column-wise
#' and total percentages.
#'
#' @param x           a matrix or data frame with two or three columns.
#' @param print       a character string or character vector indicating which percentage(s) to be printed on the
#'                    console, i.e., no percentages (\code{"no"}) (default), all percentages (\code{"all"}),
#'                    row-wise percentages (\code{"row"}), column-wise percentages (\code{"col"}), and
#'                    total percentages (\code{"total"}).
#' @param freq        logical: if \code{TRUE}, absolute frequencies will be included in the cross tabulation.
#' @param split       logical: if \code{TRUE}, output table is split in absolute frequencies and percentage(s).
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed before conducting the analysis
#'                    (i.e., listwise deletion).
#' @param digits      an integer indicating the number of decimal places digits to be used for displaying percentages.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is printed on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' \code{\link{freq}}, \code{\link{descript}}, \code{\link{multilevel.descript}},
#' \code{\link{na.descript}}.
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{crosstab}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(1, 2, 2, 1, 1, 2, 2, 1, 1, 2),
#'                   x2 = c(1, 2, 2, 1, 2, 1, 1, 1, 2, 1),
#'                   x3 = c(-99, 2, 1, 1, 1, 2, 2, 2, 2, 1), stringsAsFactors = FALSE)
#'
#' # Cross Tabulation for x1 and x2
#' crosstab(dat[, c("x1", "x2")])
#'
#' # Cross Tabulation for x1 and x2
#' # print all percentages
#' crosstab(dat[, c("x1", "x2")], print = "all")
#'
#' # Cross Tabulation for x1 and x2
#' # print row-wise percentages
#' crosstab(dat[, c("x1", "x2")], print = "row")
#'
#' # Cross Tabulation for x1 and x2
#' # print col-wise percentages
#' crosstab(dat[, c("x1", "x2")], print = "col")
#'
#' # Cross Tabulation x1 and x2
#' # print total percentages
#' crosstab(dat[, c("x1", "x2")], print = "total")
#'
#' # Cross Tabulation for x1 and x2
#' # print all percentages, split output table
#' crosstab(dat[, c("x1", "x2")], print = "all", split = TRUE)
#'
#' # Cross Tabulation for x1 and x3
#' # do not apply listwise deletion, convert value -99 to NA
#' crosstab(dat[, c("x1", "x3")], na.omit = FALSE, as.na = -99)
#'
#' # Cross Tabulation for x1 and x3
#' # print all percentages, do not apply listwise deletion, convert value -99 to NA
#' crosstab(dat[, c("x1", "x3")], print = "all", na.omit = FALSE, as.na = -99)
#'
#' # Cross Tabulation for x1, x2, and x3
#' crosstab(dat[, c("x1", "x2", "x3")])
#'
#' # Cross Tabulation for x1, x2, and x3
#' # print all percentages
#' crosstab(dat[, c("x1", "x2", "x3")], print = "all")
#'
#' # Cross Tabulation for x1, x2, and x3
#' # print all percentages, split output table
#' crosstab(dat[, c("x1", "x2", "x3")], print = "all", split = TRUE)
crosstab <- function(x, print = c("no", "all", "row", "col", "total"), freq = TRUE, split = FALSE,
                     na.omit = TRUE,
                     digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specifiy a matrix or data frame for the argumen 'x'.", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specifiy a matrix or data frame for the argumen 'x'.", call. = FALSE)

  }

  #----------------------------------------
  # Data frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  # Number of variables
  x.ncol <- ncol(x)

  #-----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x <- misty::as.na(x, as.na = as.na)

    #......
    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(x.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    #......
    # Zero variance
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1))
    if (any(x.zero.var)) {

      stop(paste0("After converting user-missing values into NA, following variables have only one unique value: ",
                  paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x'
    if (ncol(x) > 3L || ncol(x) < 2L) {

      stop("Please specify a matrix or data frame with two or three columns for the argument 'x'.", call. = FALSE)

    }

    #......
    # Check input 'x'
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))
    if (any(x.zero.var)) {

      stop(paste0("Following variables have only one unique value: ", paste(names(which(x.zero.var)), collapse = ", ")),
           call. = FALSE)

    }

    #......
    # Check input 'print'
    if (any(!print %in% c("no", "all", "row", "col", "total"))) {

      stop("Character strings in the argument 'print' do not match with \"no\", \"all\", \"row\", \"col\" or \"total\".",
           call. = FALSE)

    }

    #......
    # Check input 'freq'
    if (isFALSE(isTRUE(freq) || isFALSE(freq))) {

      stop("Please specify TRUE or FALSE for the argument 'freq'.", call. = FALSE)

    }

    #......
    # Check print = "no" and freq = FALSE
    if (all(print == "no") && isFALSE(freq)) {

      stop("Please include either percentages (i.e., print != 'no') or absolute frequencies (i.e., freq = TRUE) in the cross tabulation.",
             call. = FALSE)

    }

    #......
    # Check input 'na.omit'
    if (isFALSE(isTRUE(na.omit) || isFALSE(na.omit))) {

      stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1L != 0L | digits < 0L) {

      warning("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) | isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  # Argument print
  if (all(c("no", "all", "row", "col", "total") %in% print)) { print <- "no" }

  if (length(print) == 1 && print == "all") { print <- c("row", "col", "total") }

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # Two variables

  if (x.ncol == 2L) {

    # If na.omit = FALSE, then include NA if any present
    if (isFALSE(na.omit)) {

      x <- data.frame(lapply(x, function(x) misty::rec(x, "NA = 'NA'")), stringsAsFactors = FALSE)

    } else {

      if (any(is.na(x))) {

        warning(paste0("Listwise deletion of incomplete cases, number of cases removed from the analysis: ",
                length(attributes(na.omit(x))$na.action)), call. = FALSE)

      }

    }

    freq.a <- table(x)

    perc.r <- prop.table(freq.a, margin = 1L) * 100L

    perc.c <- prop.table(freq.a, margin = 2L) * 100L

    perc.t <- prop.table(freq.a) * 100L

  }

  #-----------------------------------------
  # Three variables

  if (x.ncol == 3L) {

    # If na.omit = FALSE, then include NA if any present
    if (isFALSE(na.omit)) {

      x <- data.frame(lapply(x, function(x) misty::rec(x, "NA = 'NA'")), stringsAsFactors = FALSE)

    } else {

      if (any(is.na(x))) {

        warning(paste0("Listwise deletion of incomplete cases, number of cases removed from the analysis: ",
                       length(attributes(na.omit(x))$na.action)), call. = FALSE)

      }

    }

    # Absolute frequencies
    x.table <- table(x[, rev(names(x))])

    freq.list <- list()
    for (i in seq_len(dim(x.table)[3L])) {

      freq.list[[i]] <- x.table[, , i]

    }

    freq.a <- freq.list
    names(freq.a) <- dimnames(x.table)[[3L]]

    # Row
    perc.r <- lapply(freq.a, function(y) prop.table(y, margin = 1L) * 100L)

    # Column %
    perc.c <- lapply(freq.a, function(y) prop.table(y, margin = 2L) * 100L)

    # Total %
    x.prop.table <- prop.table(table(x[, rev(names(x))]))

    prop.list <- list()
    for (i in seq_len(dim(x.prop.table)[3])) {

      prop.list[[i]] <- x.prop.table[, , i]*100L

    }

    perc.t <- prop.list
    names(perc.t) <- dimnames(x.prop.table)[[3L]]

  }

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 data = x,
                 args = list(freq = freq, print = print, split = split, na.omit = na.omit,
                             digits = digits, as.na = as.na, check = check, output = output),
                 result = list(freq.a = freq.a, perc.r = perc.r, perc.c = perc.c, perc.t = perc.t))

  class(object) <- "crosstab"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
