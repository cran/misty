#' Descriptive Statistics
#'
#' This function computes summary statistics for one or more than one variable,
#' optionally by a grouping and/or split variable.
#'
#' @param data     a numeric vector or data frame with numeric variables, i.e.,
#'                 factors and character variables are excluded from \code{data}
#'                 before conducting the analysis.
#' @param ...      an expression indicating the variable names in \code{data},
#'                 e.g., \code{descript(dat, x1, x2, x3)}. Note that the operators
#'                 \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                 and \code{!} can also be used to select variables, see 'Details'
#'                 in the \code{\link{df.subset}} function.
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
#'                 Alternatively, a character string indicating the variable name
#'                 of the grouping variable in \code{data} can be specified.
#' @param split    a numeric vector, character vector or factor as split variable.
#'                 Alternatively, a character string indicating the variable name
#'                 of the split variable in \code{data} can be specified.
#' @param sample   logical: if \code{TRUE} (default), the univariate sample skewness
#'                 or kurtosis is computed, while the population skewness or kurtosis
#'                 is computed when \code{sample = FALSE}.
#' @param sort.var logical: if \code{TRUE}, output table is sorted by variables when
#'                 specifying \code{group}.
#' @param na.omit  logical: if \code{TRUE}, incomplete cases are removed before
#'                 conducting the analysis (i.e., listwise deletion).
#' @param digits   an integer value indicating the number of decimal places to be
#'                 used.
#' @param as.na    a numeric vector indicating user-defined missing values,
#'                 i.e. these values are converted to \code{NA} before conducting
#'                 the analysis. Note that \code{as.na()} function is only applied
#'                 to \code{data}, but not to \code{group} or \code{split}.
#' @param write    a character string naming a file for writing the output into
#'                 either a text file with file extension \code{".txt"} (e.g.,
#'                 \code{"Output.txt"}) or Excel file with file extension
#'                 \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                 name does not contain any file extension, an Excel file will
#'                 be written.
#' @param append   logical: if \code{TRUE} (default), output will be appended
#'                 to an existing text file with extension \code{.txt} specified
#'                 in \code{write}, if \code{FALSE} existing text file will be
#'                 overwritten.
#' @param check    logical: if \code{TRUE} (default), argument specification is checked.
#' @param output   logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{ci.mean}}, \code{\link{ci.mean.diff}}, \code{\link{ci.median}},
#' \code{\link{ci.prop}}, \code{\link{ci.prop.diff}}, \code{\link{ci.var}},
#' \code{\link{ci.sd}}, \code{\link{ci.cor}}, \code{\link{freq}},
#' \code{\link{crosstab}}, \code{\link{multilevel.descript}}, \code{\link{na.descript}}.
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{list with the input specified in \code{data}, \code{group}, and \code{split}}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Descriptive statistics
#'
#' # Example 1a: Descriptive statistics for 'mpg', 'cyl', and 'hp'
#' descript(mtcars, mpg, cyl, hp)
#'
#' # Alternative specification without using the '...' argument
#' descript(mtcars[, c("mpg", "cyl", "hp")])
#'
#' # Example 1b: Print all available statistical measures
#' descript(mtcars, mpg, cyl, hp, print = "all")
#'
#' # Example 1c: Print default statistical measures plus median
#' descript(mtcars, mpg, cyl, hp, print = c("default", "med"))
#'
#' #----------------------------------------------------------------------------
#' # Grouping and Split Variable
#'
#' # Example 2a: Grouping variable
#' descript(mtcars, mpg, cyl, hp, group = "vs")
#'
#' # Alternative specification without using the '...' argument
#' descript(mtcars[, c("mpg", "cyl", "hp")], group = mtcars$vs)
#'
#' # Example 2b: Split variable
#' descript(mtcars, mpg, cyl, hp, split = "am")
#'
#' # Alternative specification without using the '...' argument
#' descript(mtcars[, c("mpg", "cyl", "hp")], split = mtcars$am)
#'
#' # Example 2c: Grouping and split variable
#' descript(mtcars, mpg, cyl, hp, group = "vs", split = "am")
#'
#' # Alternative specification without using the '...' argument
#' descript(mtcars[, c("mpg", "cyl", "hp")], group = mtcars$vs, split = mtcars$am)
#'
#' \dontrun{
#' #----------------------------------------------------------------------------
#' # Write Output
#'
#' # Example 3a: Text file
#' descript(mtcars, write = "Descript_Text.txt")
#'
#' # Example 3b: Excel file
#' descript(mtcars, write = "Descript_Excel.xlsx")
#' }
descript <- function(data, ...,
                     print = c("all", "default", "n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt"),
                     group = NULL, split = NULL, sample = FALSE, sort.var = FALSE, na.omit = FALSE,
                     digits = 2, as.na = NULL, write = NULL, append = TRUE,
                     check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a numeric vector or data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- data[, .var.names(..., data = data, group = group, split = split), drop = FALSE] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

    # Splitting variable
    if (isTRUE(!is.null(split))) { split <- data[, split] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

    # Data and cluster
    var.group <- .var.group(data = x, group = group, split = split)

    # Data
    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }

    # Grouping variable
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }

    # Split variable
    if (isTRUE(!is.null(var.group$split))) { split <- var.group$split }

  }

  # Convert 'group' and 'split' as tibble into a vector
  if (!is.null(group) && isTRUE("tbl" %in% substr(class(group), 1L, 3L))) { group <- unname(unlist(group)) }
  if (!is.null(split) && isTRUE("tbl" %in% substr(class(split), 1L, 3L))) { split <- unname(unlist(split)) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Numeric Variables ####

  x <- x |> (\(y) !vapply(y, is.numeric, FUN.VALUE = logical(1L)))() |> (\(z) if (isTRUE(any(z))) {

    warning(paste0("Non-numeric variables were excluded from the analysis: ", paste(names(which(z)), collapse = ", ")), call. = FALSE)

    return(x[, -which(z), drop = FALSE])

  } else {

    return(x)

  })()

  if (isTRUE(ncol(x) == 0L)) { stop("No variables left for analysis after excluding non-numeric variables.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping and Split Variable ####

  # Grouping variable
  if (!is.null(group)) {

    x <- which(sapply(names(x), function(y) identical(group, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the grouping variable from the data frame, there are no variables left.") }

  }

  # Split variable
  if (!is.null(split)) {

    x <- which(sapply(names(x), function(y) identical(split, x[, y]))) |> (\(z) if (isTRUE(length(z) != 0L)) { return(x[, -z]) } else { x })()

    if (isTRUE(ncol(x) == 0L)) { stop("After excluding the split variable from the data frame, there are no variables left.") }

  }

  # Grouping and split variable are identical
  if (isTRUE(!is.null(group) && !is.null(split) && identical(group, split))) { stop("Grouping and split variables are identical.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Listwise deletion ####

  # Check input 'na.omit'
  .check.input(logical = "na.omit", envir = environment(), input.check = check)

  if (isTRUE(na.omit && any(is.na(x)))) {

    assign("x", na.omit(x)) |> (\(y) warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(y)$na.action)), call. = FALSE))()

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- group[-attributes(na.omit(x))$na.action] }

    # Split variable
    if (isTRUE(!is.null(split))) { split <- split[-attributes(na.omit(x))$na.action] }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("sample", "sort.var", "na.omit", "append", "output"),
               m.character = list(print = c("all", "default", "n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")),
               args = c("digits", "write2"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

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

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Statistical measures ####

  print.all <- c("n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt")

  # Default setting
  if (isTRUE(all(c("all", "default", "n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt") %in% print))) {

    print <- c("n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")

  # All statistical measures
  } else if (isTRUE("all" %in% print)) {

    print <- print.all

  # Default setting with additional statistical measures
  } else if (isTRUE("default" %in% print && length(print > 1L))) {

    print <- print.all[print.all %in% misty::chr.omit(union(c("n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt"), print), "default", check = FALSE)]

  # Manual default setting
  } else if (isTRUE(all(print == "default"))) {

    print <- c("n", "nNA", "pNA", "m", "sd", "min", "max", "skew", "kurt")

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
                         skew = suppressWarnings(vapply(x, misty::skewness, sample = sample, check = FALSE, FUN.VALUE = double(1L))),
                         kurt = suppressWarnings(vapply(x, misty::kurtosis, sample = sample, check = FALSE, FUN.VALUE = double(1L))),
                         row.names = NULL, check.names = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, No Split ####

  } else if (isTRUE(!is.null(group) && is.null(split))) {

    result <- lapply(split(x, f = group), function(y) misty::descript(y, group = NULL, split = NULL, sort.var = sort.var, check = FALSE, output = FALSE)$result) |> (\(y) data.frame(group = rep(names(y), each = ncol(x)), eval(parse(text = paste0("rbind(", paste0("y[[", seq_len(length(y)), "]]", collapse = ", "), ")")))) )()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####

  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x), f = split), function(y) misty::descript(y, group = NULL, split = NULL, sort.var = sort.var, check = FALSE, output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####

  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, group = group), f = split), function(y) misty::descript(y[, -grep("group", names(y))], group = y$group, split = NULL, sort.var = sort.var, check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "descript",
                 data = list(x = x, group = group, split = split),
                 args = list(print = print, sample = sample, sort.var = sort.var, na.omit = na.omit, digits = digits, as.na = as.na, write = write, append = append, check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

  if (isTRUE(!is.null(write))) { .write.result(object = object, write = write, append = append) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}

#_______________________________________________________________________________
