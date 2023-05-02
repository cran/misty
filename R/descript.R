#' Descriptive Statistics
#'
#' This function computes summary statistics for one or more variables, optionally
#' by a grouping and/or split variable.
#'
#' @param x        a numeric vector, matrix or data frame with numeric variables,
#'                 i.e., factors and character variables are excluded from \code{x}
#'                 before conducting the analysis.
#' @param print    a character vector indicating which statistical measures to be
#'                 printed on the console, i.e. \code{n} (number of observations),
#'                 \code{nNA} (number of missing values), \code{pNA} (percentage of
#'                 missing values), \code{m} (arithmetic mean), \code{se.m} (standard
#'                 error of the arithmetic mean), \code{var} (variance), \code{sd}
#'                 (standard deviation), \code{med} (median),\code{min} (minimum),
#'                 \code{p25} (25th percentile, first quartile), \code{p75} (75th
#'                 percentile, third quartile), \code{max} (maximum),  \code{range}
#'                 (range), \code{iqr} (interquartile range), \code{skew} (skewness),
#'                 and \code{kurt} (excess kurtosis). The default setting is
#'                 \code{print = ("n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")}.
#' @param group    a numeric vector, character vector or factor as grouping variable.
#' @param split    a numeric vector, character vector or factor as split variable.
#' @param sort.var logical: if \code{TRUE}, output table is sorted by variables when
#'                 specifying \code{group}.
#' @param na.omit  logical: if \code{TRUE}, incomplete cases are removed before
#'                 conducting the analysis (i.e., listwise deletion).
#' @param digits   an integer value indicating the number of decimal places to be
#'                 used.
#' @param as.na    a numeric vector indicating user-defined missing values,
#'                 i.e. these values are converted to \code{NA} before conducting
#'                 the analysis. Note that \code{as.na()} function is only applied
#'                 to \code{x}, but not to \code{group} or \code{split}.
#' @param write     a character string for writing the results into a Excel file
#'                  naming a file with or without file extension '.xlsx', e.g.,
#'                  \code{"Results.xlsx"} or \code{"Results"}.
#' @param check    logical: if \code{TRUE}, argument specification is checked.
#' @param output   logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{ci.mean}}, \code{\link{ci.mean.diff}}, \code{\link{ci.median}},
#' \code{\link{ci.prop}}, \code{\link{ci.prop.diff}}, \code{\link{ci.var}},
#' \code{\link{ci.sd}}, \code{\link{freq}}, \code{\link{crosstab}},
#' \code{\link{multilevel.descript}}, \code{\link{na.descript}}.
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
#' \code{data} \tab list with the input specified in \code{x}, \code{group}, and
#'                  \code{split} \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list with result tables \cr
#' }
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
#'
#' \dontrun{
#' # Write Results into a Excel file
#' descript(dat[, c("x1", "x2", "x3")], write = "Descript.xlsx")
#'
#' result <- descript(dat[, c("x1", "x2", "x3")], output = FALSE)
#' write.result(result, "Descript.xlsx")
#' }
descript <- function(x, print = c("all", "n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt"),
                     group = NULL, split = NULL, sort.var = FALSE, na.omit = FALSE,
                     digits = 2, as.na = NULL, write = NULL, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector, matrix or data frame with numeric variables for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check 'group'
  if (isTRUE(!is.null(group))) {

    if (ncol(data.frame(group)) != 1L) { stop("More than one grouping variable specified for the argument 'group'.",call. = FALSE) }

    if (nrow(data.frame(group)) != nrow(data.frame(x))) { stop("Length of the vector or factor specified in the argument 'group' does not match with 'x'.", call. = FALSE) }

    # Convert 'group' into a vector
    group <- unlist(group, use.names = FALSE)

  }

  # Check 'split'
  if (isTRUE(!is.null(split))) {

    if (ncol(data.frame(split)) != 1L) { stop("More than one split variable specified for the argument 'split'.",call. = FALSE) }

    if (nrow(data.frame(split)) != nrow(data.frame(x))) { stop("Length of the vector or factor specified in the argument 'split' does not match with 'x'.", call. = FALSE) }

    # Convert 'split' into a vector
    split <- unlist(split, use.names = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data frame ####

  # Is 'x' a matrix?
  is.mat <- is.matrix(x) && !is.data.frame(x)

  # Coerce to a data frame
  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    # Replace user-specified values with missing values
    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ", paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  # Non-numeric variables
  non.num <- !vapply(x, is.numeric, FUN.VALUE = logical(1L))

  if (isTRUE(any(non.num))) {

    x <- x[, -which(non.num), drop = FALSE]

    # Variables left
    if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(non.num)), collapse = ", ")), call. = FALSE)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  if (isTRUE(na.omit) && any(is.na(x))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Grouping, No Split ####
    if (isTRUE(is.null(group) && is.null(split))) {

      x <- na.omit(as.data.frame(x, stringsAsFactors = FALSE))

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x)$na.action)), call. = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping, No Split ####
    if (isTRUE(!is.null(group) && is.null(split))) {

      x.group <- na.omit(data.frame(x, group = group, stringsAsFactors = FALSE))

      x <- x.group[, -grep("group", names(x.group)), drop = FALSE]
      group <- x.group$group

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x.group)$na.action)), call. = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## No Grouping, Split ####
    if (isTRUE(is.null(group) && !is.null(split))) {

      x.split <- na.omit(data.frame(x, split = split, stringsAsFactors = FALSE))

      x <- x.split[, -grep("split", names(x.split)), drop = FALSE]
      split <- x.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ",
                     length(attributes(x.split)$na.action)), call. = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Grouping, Split ####
    if (isTRUE(!is.null(group) && !is.null(split))) {

      x.group.split <- na.omit(data.frame(x, group = group, split = split, stringsAsFactors = FALSE))

      x <- x.group.split[,  !names(x.group.split) %in% c("group", "split"), drop = FALSE]
      group <- x.group.split$group
      split <- x.group.split$split

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x.group.split)$na.action)), call. = FALSE)

    }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Variable with missing values only ####
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After listwise deletion, following variables are completely missing: ", paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'print'
    if (isTRUE(!all(print %in% c("all", "n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "skew", "range", "iqr", "kurt")))) {

      stop("Character strings in the argument 'print' do not all match with \"all\", \"n\", \"nNA\", \"pNA\", \"m\", \"se.m\", \"var\", \"sd\", \"min\", \"p25\", \"med\", \"p75\", \"max\", \"range\", \"iqr\", \"skew\", or \"kurt\".", call. = FALSE)

    }

    # Check input 'group'
    if (isTRUE(!is.null(group))) {

      # Input 'group' completely missing
      if (isTRUE(all(is.na(group)))) { stop("The grouping variable specified in 'group' is completely missing.", call. = FALSE) }

      # Only one group in 'group'
      if (isTRUE(length(na.omit(unique(group))) == 1L)) { warning("There is only one group represented in the grouping variable specified in 'group'.", call. = FALSE) }

    }

    # Check input 'split'
    if (isTRUE(!is.null(split))) {

      # Input 'split' completely missing
      if (isTRUE(all(is.na(split)))) { stop("The split variable specified in 'split' is completely missing.", call. = FALSE) }

      # Only one group in 'split'
      if (isTRUE(length(na.omit(unique(split))) == 1L)) { warning("There is only one group represented in the split variable specified in 'split'.", call. = FALSE) }

    }

    # Check input 'sort.var'
    if (isTRUE(!is.logical(sort.var))) { stop("Please specify TRUE or FALSE for the argument 'sort.var'.", call. = FALSE) }

    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) { stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Statistical measures ####

  if (isTRUE(all(c("all", "n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt") %in% print))) {

    print <- c("n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")

  }

  if (isTRUE(length(print) == 1L && print == "all")) {

    print <- c("n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, No Split ####

  if (isTRUE(is.null(group) && is.null(split))) {

    result <- data.frame(variable = colnames(x),
                         n = vapply(x, function(y) length(na.omit(y)), FUN.VALUE = 1L),
                         nNA = vapply(x, function(y) sum(is.na(y)), FUN.VALUE = 1L),
                         pNA = vapply(x, function(y) sum(is.na(y)) / length(y) * 100L, FUN.VALUE = double(1)),
                         m = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, mean(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         se.m = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, sd(y, na.rm = TRUE) / sqrt(length(na.omit(y)))), FUN.VALUE = double(1L)),
                         var = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, var(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         sd = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, sd(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         min = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, min(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         p25 = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, quantile(y, probs = 0.25, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         med = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, median(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         p75 = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, quantile(y, probs = 0.75, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         max = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, max(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         range = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, diff(range(y, na.rm = TRUE))), FUN.VALUE = double(1L)),
                         iqr = vapply(x, function(y) ifelse(length(na.omit(y)) <= 1L, NA, IQR(y, na.rm = TRUE)), FUN.VALUE = double(1L)),
                         skew = suppressWarnings(vapply(x, misty::skewness, check = FALSE, FUN.VALUE = double(1L))),
                         kurt = suppressWarnings(vapply(x, misty::kurtosis, check = FALSE, FUN.VALUE = double(1L))),
                         stringsAsFactors = FALSE, row.names = NULL, check.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####
  } else if (isTRUE(!is.null(group) && is.null(split))) {

    object.group <- lapply(split(x, f = group), function(y) misty::descript(y, group = NULL, split = NULL, sort.var = sort.var, na.omit = na.omit,
                                                                            as.na = as.na, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = ncol(x)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))),
                         stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####
  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, stringsAsFactors = FALSE), f = split),
                     function(y) misty::descript(y, group = NULL, split = NULL, sort.var = sort.var,
                                                 na.omit = na.omit, as.na = as.na, check = FALSE,
                                                 output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####
  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, group = group, stringsAsFactors = FALSE), f = split),
                     function(y) misty::descript(y[, -grep("group", names(y))], group = y$group, split = NULL,
                                                 sort.var = sort.var, na.omit = na.omit, as.na = as.na,
                                                 check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "descript",
                 data = list(x = x, group = group, split = split),
                 args = list(print = print, sort.var = sort.var, na.omit = na.omit,
                             digits = digits, as.na = as.na, check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { misty::write.result(object, file = write) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
