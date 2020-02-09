#' Descriptive Statistics
#'
#' This function computes summary statistics for one or more variables optionally by a grouping variable.
#'
#' @param x           a numeric vector, matrix or data frame with numeric variables, i.e.,
#'                    factors and character variables are excluded from \code{x} before conducting the analysis.
#' @param print       a character vector indicating which statistical measures to be printed on the console,
#'                    i.e. \code{n} (number of observations), \code{nNA} (number of missing values),
#'                    \code{pNA} (percentage of missing values), \code{m} (arithmetic mean), \code{var} (variance),
#'                    \code{sd} (standard deviation), \code{med} (median),\code{min} (minimum),
#'                    \code{p25} (25th percentile, first quartile), \code{p75} (75th percentile, third quartile),
#'                    \code{max} (maximum),  \code{range} (range), \code{iqr} (interquartile range),
#'                    \code{skew} (skewness), and \code{kurt} (excess kurtosis).
#'                    The default setting is \code{print = ("n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")}.
#' @param group       a numeric vector, character vector or factor as grouping variable.
#' @param split       a numeric vector, character vector or factor as split variable.
#' @param sort.var    logical: if \code{TRUE}, output table is sorted by variables when specifying \code{group}.
#' @param na.omit     logical: if \code{TRUE}, incomplete cases are removed before conducting the analysis
#'                    (i.e., listwise deletion).
#' @param digits      an integer value indicating the number of decimal places to be used.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#'                    Note that \code{as.na()} function is only applied to \code{x}, but
#'                    not to \code{group} or \code{split}.
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
#' dat <- data.frame(group1 = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2),
#'                   group2 = c(1, 1, 1, 2, 2, 2, 1, 1, 1, 2, 2, 2),
#'                   x1 = c(3, 1, 4, 2, 5, 3, 2, 4, NA, 4, 5, 3),
#'                   x2 = c(4, NA, 3, 6, 3, 7, 2, 7, 5, 1, 3, 6),
#'                   x3 = c(7, 8, 5, 6, 4, NA, 8, NA, 6, 5, 8, 6))
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
#' # analysis by group1 separately
#' descript(dat[, c("x1", "x2", "x3")], group = dat$group1)
#'
#' # Descriptive statistics for x1, x2, and x3,
#' # analysis by group1 separately, sort by variables
#' descript(dat[, c("x1", "x2", "x3")], group = dat$group1, sort.var = TRUE)
#'
#' # Descriptive statistics for x1, x2, and x3,
#' # split analysis by group1
#' descript(dat[, c("x1", "x2", "x3")], split = dat$group1)
#'
#' # Descriptive statistics for x1, x2, and x3,
#' # analysis by group1 separately, split analysis by group2
#' descript(dat[, c("x1", "x2", "x3")], group = dat$group1, split = dat$group2)
descript <- function(x, print = c("all", "n", "nNA", "pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt"),
                     group = NULL, split = NULL, sort.var = FALSE, na.omit = FALSE,
                     digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Vector, matrix or data frame for the argument 'x'?
  if (!is.vector(x) && !is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.",
         call. = FALSE)

  }

  #----------------------------------------
  # Data frame

  x <- as.data.frame(x)

  #----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x <- misty::as.na(x, as.na = as.na, check = FALSE)

    if (isFALSE(all(vapply(x, function(y) !as.na %in% y, FUN.VALUE = logical(1))))) {

      warning("Value(s) specified in the argument 'as.na' were not found in 'x'.", call. = FALSE)

    }

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(x.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #----------------------------------------
  # Numeric Variables

  # Non-numeric variables
  non.num <- !vapply(x, is.numeric, FUN.VALUE = logical(1))

  if (any(non.num)) {

    x <- x[, -which(non.num), drop = FALSE]

    #......
    # Variables left

    if (ncol(x) == 0) {

      stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE)

    }

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(non.num)), collapse = ", ")),
            call. = FALSE)

  }

  #----------------------------------------
  # Listwise deletion

  if (isTRUE(na.omit) && any(is.na(x))) {

    #......
    # No group and split variable
    if (is.null(group) && is.null(split)) {

      x <- na.omit(as.data.frame(x))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x)$na.action)), call. = FALSE)

    }

    #......
    # Group variable, no split variable
    if (!is.null(group) && is.null(split)) {

      x.group <- na.omit(data.frame(x, group = group))

      x <- x.group[, -grep("group", names(x.group)), drop = FALSE]
      group <- x.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.group)$na.action)), call. = FALSE)

    }

    #......
    # No group variable, split variable
    if (is.null(group) && !is.null(split)) {

      x.split <- na.omit(data.frame(x, split = split))

      x <- x.split[, -grep("split", names(x.split)), drop = FALSE]
      split <- x.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.split)$na.action)), call. = FALSE)

    }

    #......
    # Group variable, split variable
    if (!is.null(group) && !is.null(split)) {

      x.group.split <- na.omit(data.frame(x, group = group, split = split))

      x <- x.group.split[,  !names(x.group.split) %in% c("group", "split"), drop = FALSE]
      group <- x.group.split$group
      split <- x.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.group.split)$na.action)), call. = FALSE)

    }

    #......
    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(x.miss)) {

      stop(paste0("After listwise deletion, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'print'
    if (!all(print %in%  c("all", "n", "nNA", "pNA", "m", "var", "sd", "min", "p25", "med", "p75", "max", "skew", "range", "iqr", "kurt"))) {

      stop("Character strings in the argument 'print' do not all match with \"all\", \"n\", \"nNA\", \"pNA\", \"m\", \"var\", \"sd\", \"min\", \"p25\", \"med\", \"p75\", \"max\", \"range\", \"iqr\", \"skew\", or \"kurt\".",
           call. = FALSE)

    }

    #......
    # Check input 'group'
    if (!is.null(group)) {

      # Vector or factor for the argument 'group'?
      if (!is.vector(group) && !is.factor(group)) {

        stop("Please specify a vector or factor for the argument 'group'.", call. = FALSE)

      }

      # Length of 'group' match with 'x'?
      if (length(group) != nrow(x)) {

        if (ncol(x) == 1) {

          stop("Length of the vector or factor specified in 'group' does not match the length of the vector specified in 'x'.",
               call. = FALSE)

        } else {

          stop("Length of the vector or factor specified in 'group' does not match the number of rows of the matrix or data frame specified in 'x'.",
               call. = FALSE)

        }

      }

      # Input 'group' completely missing
      if (all(is.na(group))) {

        stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE)

      }

      # Only one group in 'group'
      if (length(na.omit(unique(group))) == 1) {

        warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE)

      }

    }

    #......
    # Check input 'split'
    if (!is.null(split)) {

      # Vector or factor for the argument 'split'?
      if (!is.vector(split) && !is.factor(split)) {

        stop("Please specify a vector or factor for the argument 'split'.", call. = FALSE)

      }

      # Length of 'split' doest not match with 'x'
      if (length(split) != nrow(x)) {

        if (ncol(x) == 1) {

          stop("Length of the vector or factor specified in 'split' does not match the length of the vector specified in 'x'.",
               call. = FALSE)

        } else {

          stop("Length of the vector or factor specified in 'split' does not match the number of rows of the matrix or data frame specified in 'x'.",
               call. = FALSE)

        }

      }

      # Input 'split' completely missing
      if (all(is.na(split))) {

        stop("The grouping variable specified in 'split' is completely missing.", call. = FALSE)

      }

      # Only one group in 'split'
      if (length(na.omit(unique(split))) == 1) {

        warning("There is only one group represented in the grouping variable specified in 'split'.", call. = FALSE)

      }

    }

    #......
    # Check input 'sort.var'
    if (isFALSE(isTRUE(sort.var) || isFALSE(sort.var))) {

      stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE)

    }

    #......
    # Check input 'na.omit'
    if (isFALSE(isTRUE(na.omit) || isFALSE(na.omit))) {

      stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 || digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input output
    if (isFALSE(isTRUE(output) || isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

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

    result <-  data.frame(variable = colnames(x),
                          n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1),
                          nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1),
                          pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100, FUN.VALUE = double(1)),
                          m = vapply(x, mean, na.rm = TRUE, FUN.VALUE = double(1)),
                          var = vapply(x, var, na.rm = TRUE, FUN.VALUE = double(1)),
                          sd = vapply(x, sd, na.rm = TRUE, FUN.VALUE = double(1)),
                          min = vapply(x, min, na.rm = TRUE, FUN.VALUE = double(1)),
                          p25 = vapply(x, quantile, probs = 0.25, na.rm = TRUE, FUN.VALUE = double(1)),
                          med = vapply(x, median, na.rm = TRUE, FUN.VALUE = double(1)),
                          p75 = vapply(x, quantile, probs = 0.75, na.rm = TRUE, FUN.VALUE = double(1)),
                          max = vapply(x, max, na.rm = TRUE, FUN.VALUE = double(1)),
                          range = vapply(x, function(y) diff(range(y, na.rm = TRUE)), FUN.VALUE = double(1)),
                          iqr = vapply(x, IQR, na.rm = TRUE, FUN.VALUE = double(1)),
                          skew = suppressWarnings(vapply(x, misty::skewness, check = FALSE, FUN.VALUE = double(1))),
                          kurt = suppressWarnings(vapply(x, misty::kurtosis, check = FALSE, FUN.VALUE = double(1))),
                          stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

  }

  #----------------------------------------
  # Grouping, no split

  if (!is.null(group) && is.null(split)) {

    object.group <- lapply(split(x, f = group), function(y) misty::descript(y, group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                                            as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = ncol(x)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))),
                         stringsAsFactors = FALSE)

  }

  #----------------------------------------
  # Split

  if (!is.null(split)) {

    # No grouping
    if (is.null(group)) {

      result <- lapply(split(data.frame(x), f = split), function(y) misty::descript(y, group = NULL, split = NULL, sort.var = sort.var,
                                                                                    na.omit = na.omit, as.na = as.na, check = FALSE,
                                                                                    output = FALSE)$result)

    # Grouping
    } else {

      result <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::descript(y[, -grep("group", names(y))], group = y$group, split = NULL,
                                                                                                   sort.var = sort.var, na.omit = na.omit, as.na = as.na,
                                                                                                   check = FALSE, output = FALSE)$result)

    }

  }

  ####################################################################################
  # Return object and output

  object <- list(call = match.call(),
                 data = list(x = x, group = group, split = split),
                 args = list(group = group, split = split, sort.var = sort.var,
                             print = print, digits = digits, as.na = as.na, check = check, output = output),
                 result = result)

  class(object) <- "descript"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
