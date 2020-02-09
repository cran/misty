#' Descriptive Statistics
#'
#' This function computes summary statistics for one or more variables optionally by a grouping variable.
#'
#' @param x           a numeric vector, matrix or data frame with numeric variables, i.e., factors and character
#'                    variables are excluded from \code{x}before conducting the analysis.
#' @param print       a character vector indicating which statistical measures to be printed on the console,
#'                    i.e. \code{n} (number of observations), \code{nNA} (number of missing values),
#'                    \code{pNA} (percentage of missing values), \code{m} (arithmetic mean), \code{var} (variance),
#'                    \code{sd} (standard deviation), \code{med} (median),\code{min} (minimum),
#'                    \code{p25} (25th percentile, first quartile), \code{p75} (75th percentile, third quartile),
#'                    \code{max} (maximum),  \code{range} (range), \code{iqr} (interquartile range),
#'                    \code{skew} (skewness), and \code{kurt} (excess kurtosis).
#'                    The default setting is \code{print = ("n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")}.
#' @param group       a numeric vector, character vector or factor as grouping variable.
#' @param mat         logical: if \code{TRUE}, output by a grouping variable is shown in matrix format.
#' @param sort.var    logical: if \code{TRUE}, output is sorted by variables.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed before conducting the analysis
#'                    (i.e., listwise deletion).
#' @param digits      an integer value indicating the number of decimal places to be used.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#'                    Note that \code{as.na()} function is only applied to \code{x}, but not to \code{group}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{crosstab}}, \code{\link{freq}}, \code{\link{multilevel.descript}},
#' \code{\link{na.descript}}.
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{descript}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
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
#' descript(dat$x1)
#'
#' # Descriptive statistics for x1, print results with 3 digits
#' descript(dat$x1, digits = 3)
#'
#' # Descriptive statistics for x1, convert value 4 to NA
#' descript(dat$x1, as.na = 4)
#'
#' # Descriptive statistics for x1, print all available statistical measures
#' descript(dat$x1, print = "all")
#'
#' # Descriptive statistics for x1, x2, and x3
#' descript(dat[, c("x1", "x2", "x3")])
#'
#' # Descriptive statistics for x1, x2, and x3,
#' # listwise deletion for missing data
#' descript(dat[, c("x1", "x2", "x3")], na.omit = TRUE)
#'
#' # Descriptive statistics for x1, x2, and x3,
#' # analysis by group separately
#' descript(dat[, c("x1", "x2", "x3")], group = dat$group)
#'
#' # Descriptive statistics for x1, x2, and x3,
#' # analysis by group separately, show results in matrix format
#' descript(dat[, c("x1", "x2", "x3")], group = dat$group, mat = TRUE)
#'
#' # Descriptive statistics for x1, x2, and x3,
#' # analysis by group separately, show results in matrix format, sort by variables
#' descript(dat[, c("x1", "x2", "x3")], group = dat$group, mat = TRUE, sort.var = TRUE)
descript <- function(x, print = c("all", "n", "nNA", "pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt"),
                     group = NULL, mat = FALSE, sort.var = FALSE, na.omit = FALSE,
                     digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #----------------------------------------
  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'",
         call. = FALSE)

  }

  #----------------------------------------
  # Vector, matrix or data frame for the argument 'x'?
  if (!is.vector(x) && !is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'",
         call. = FALSE)

  }

  #----------------------------------------
  # Data frame

  df <- as.data.frame(x)

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    df <- misty::as.na(df, na = as.na, check = FALSE)

    if (isTRUE(check) && isFALSE(all(sapply(df, function(y) !as.na %in% y)))) {

      warning("Value(s) specified in the argument 'na' were not found in 'x'.", call. = FALSE)

    }

    # Variable with missing values only
    df.miss <- sapply(df, function(y) all(is.na(y)))
    if (any(df.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(df.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #----------------------------------------
  # Numeric Variables

  non.num <- !sapply(df, is.numeric)

  if (any(non.num)) {

    df <- df[, -which(non.num), drop = FALSE]

    #......
    # Variables left

    if (ncol(df) == 0) {

      stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE)

    }

    if (!is.null(dim(x))) { x <- x[, -which(non.num), drop = FALSE] }

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(non.num)), collapse = ", ")),
            call. = FALSE)

  }

  #----------------------------------------
  # Listwise deletion

  if (isTRUE(na.omit) && any(is.na(df))) {

    #......
    # No grouping variable
    if (is.null(group)) {

      df <- na.omit(as.data.frame(df))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                    length(attributes(df)$na.action)), call. = FALSE)

    #......
    # Grouping variable
    } else {

      df.group <- na.omit(data.frame(df, group = group))

      df <- df.group[, -grep("group", names(df.group)), drop = FALSE]
      group <- df.group$group

      warning(paste("Listwise deletion of incomplete data, number of cases removed from the analysis:",
                    length(attributes(df.group)$na.action)), call. = FALSE)

    }

  }

  #----------------------------------------
  # Constant

  non.var <- sapply(df, function(y) var(y, na.rm = TRUE) == 0)

  if (any(non.var)) {

    df <- df[, -which(non.var), drop = FALSE]

    if (!is.null(dim(x))) { x <- x[, -which(non.var), drop = FALSE] }

    #......
    # Variables left

    if (ncol(df) == 0) {

      stop("No variables left for analysis after excluding constant variables.", call. = FALSE)

    }

    warning(paste0("Constant variables were excluded from the analysis: ", paste0(names(which(non.var)), collapse = ", ")),
            call. = FALSE)

  }

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #----------------------------------------

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
    # Check input 'na.omit'
    if (isFALSE(isTRUE(na.omit) | isFALSE(na.omit))) {

      stop("Please specify TRUE or FALSE for the argument 'na.omit'", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

    #......
    # Check input output
    if (isFALSE(isTRUE(output) | isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'", call. = FALSE)

    }

  }

  ####################################################################################
  # Arguments

  #----------------------------------------
  # Statistical measures

  if (all(c("all", "n", "nNA", "pNA", "m", "var", "sd", "min", "p25",  "med", "p75", "max", "range", "iqr", "skew", "kurt") %in% print)) {

    print <- c("n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")

  }

  if (length(print) == 1 && print == "all") {

    print <- c("n", "nNA", "pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

  }

  ####################################################################################
  # Main Function

  #----------------------------------------
  # No grouping
  if (is.null(group)) {

    # Number of valid observations
    n <- sapply(df, function(y) length(na.omit(y)))

    # Number of missing values
    nNA <- sapply(df, function(y) sum(is.na(y)))

    # Percentage missing values
    pNA <- sapply(df, function(y) sum(is.na(y)) / length(y) * 100)

    # Mean
    m <- sapply(df, mean, na.rm = TRUE)

    # Variance
    variance <- sapply(df, var, na.rm = TRUE)

    # Standard deviation
    sd <- sapply(df, sd, na.rm = TRUE)

    # Minimum
    min <- sapply(df, min, na.rm = TRUE)

    # 25th percentile
    p25 <- sapply(df, quantile, probs = 0.25, na.rm = TRUE)

    # Median
    med <- sapply(df, median, na.rm = TRUE)

    # 75th percentile
    p75 <- sapply(df, quantile, probs = 0.75, na.rm = TRUE)

    # Maximum
    max <- sapply(df, max, na.rm = TRUE)

    # Range
    range <- sapply(df, function(y) diff(range(y, na.rm = TRUE)))

    # Interquartile Range
    iqr <- sapply(df, IQR, na.rm = TRUE)

    # Skewness
    skew <- sapply(df, skewness, check = TRUE)

    # Kurtosis
    kurt <- sapply(df, kurtosis, check = TRUE)

  #----------------------------------------
  # Grouping
  } else {

    df.group <- split(df, f = group)

    object.group <- lapply(df.group, function(y) misty::descript(y, group = NULL, na.omit = na.omit, as.na = as.na, check = FALSE, output = FALSE))

  }

  ####################################################################################
  # Return object and output

  #----------------------------------------
  # No grouping
  if (is.null(group)) {

    object <- list(call = match.call(),
                   data = x,
                   args = list(group = group, mat = mat, sort.var = sort.var,
                               print = print, digits = digits, as.na = as.na, check = check, output = output),
                   result = data.frame(variable = colnames(df), n = n, nNA = nNA, pNA = pNA, m = m, var = variance, sd = sd,
                                       min = min, p25 = p25, med = med, p75 = p75, max = max, range = range,
                                       iqr = iqr, skew = skew, kurt = kurt,
                                       stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE))

  #----------------------------------------
  # Grouping
  } else {

    object <- list(call = match.call(),
                   data = x,
                   args = list(group = group, mat = mat, sort.var = sort.var,
                               print = print, digits = digits, as.na = as.na, check = check, output = output),
                   result = lapply(object.group, function(y) y$result))

  }

  class(object) <- "descript"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
