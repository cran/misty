#' Descriptive Statistics
#'
#' This function computes summary statistics for one or more than one variables,
#' optionally by a grouping and/or split variable.
#'
#' @param ...      a numeric vector, matrix or data frame with numeric variables,
#'                 i.e., factors and character variables are excluded from \code{...}
#'                 before conducting the analysis. Alternatively, an expression
#'                 indicating the variable names in \code{data} e.g.,
#'                 \code{descript(x1, x2, x3, data = dat)}. Note that the operators
#'                 \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                 and \code{!} can also be used to select variables, see 'Details'
#'                 in the \code{\link{df.subset}} function.
#' @param data     a data frame when specifying one or more variables in the
#'                 argument \code{...}. Note that the argument is \code{NULL}
#'                 when specifying a numeric vector, matrix, or data frame for
#'                 the argument \code{...}.
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
#' @param sort.var logical: if \code{TRUE}, output table is sorted by variables when
#'                 specifying \code{group}.
#' @param na.omit  logical: if \code{TRUE}, incomplete cases are removed before
#'                 conducting the analysis (i.e., listwise deletion).
#' @param digits   an integer value indicating the number of decimal places to be
#'                 used.
#' @param as.na    a numeric vector indicating user-defined missing values,
#'                 i.e. these values are converted to \code{NA} before conducting
#'                 the analysis. Note that \code{as.na()} function is only applied
#'                 to \code{...}, but not to \code{group} or \code{split}.
#' @param write    a character string naming a file for writing the output into
#'                 either a text file with file extension \code{".txt"} (e.g.,
#'                 \code{"Output.txt"}) or Excel file with file extention
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
#' \code{data} \tab list with the input specified in \code{...}, \code{group}, and
#'                  \code{split} \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab list with result tables \cr
#' }
#'
#' @export
#'
#' @examples
#' # Example 1a: Descriptive statistics for 'mpg'
#' descript(mtcars$mpg)
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' descript(mpg, data = mtcars)
#'
#' # Example 2: Descriptive statistics, print results with 3 digits
#' descript(mtcars$mpg, digits = 3)
#'
#' # Example 3: Descriptive statistics for x1, print all available statistical measures
#' descript(mtcars$mpg, print = "all")
#'
#' # Example 4a: Descriptive statistics for 'mpg', 'cyl', and 'disp'
#' descript(mtcars[, c("mpg", "cyl", "disp")])
#'
#' # Example 4b: Alternative specification using the 'data' argument
#' descript(mpg:disp, data = mtcars)
#'
#' # Example 5a: Descriptive statistics, analysis by 'vs' separately
#' descript(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs)
#'
#' # Example 5b: Alternative specification using the 'data' argument
#' descript(mpg:disp, data = mtcars, group = "vs")
#'
#' # Example 6: Descriptive statistics, analysis by 'vs' separately, sort by variables
#' descript(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs, sort.var = TRUE)
#'
#' # Example 7: Descriptive statistics, split analysis by 'am'
#' descript(mtcars[, c("mpg", "cyl", "disp")], split = mtcars$am)
#'
#' # Example 8a: Descriptive statistics,analysis by 'vs' separately, split analysis by 'am'
#' descript(mtcars[, c("mpg", "cyl", "disp")], group = mtcars$vs, split = mtcars$am)
#'
#' # Example 8b: Alternative specification using the 'data' argument
#' descript(mpg:disp, data = mtcars, group = "vs", split = "am")
#'
#' \dontrun{
#' # Example 11a: Write Results into a text file
#' descript(mtcars[, c("mpg", "cyl", "disp")], write = "Descript.txt")
#'
#' # Example 11b: Write Results into a Excel file
#' descript(mtcars[, c("mpg", "cyl", "disp")], write = "Descript.xlsx")
#'
#' result <- descript(mtcars[, c("mpg", "cyl", "disp")], output = FALSE)
#' write.result(result, "Descript.xlsx")
#' }
descript <- function(..., data = NULL,
                     print = c("all", "n", "nNA", "pNA", "m", "se.m", "var", "sd", "min", "p25", "med", "p75", "max", "range", "iqr", "skew", "kurt"),
                     group = NULL, split = NULL, sort.var = FALSE, na.omit = FALSE,
                     digits = 2, as.na = NULL, write = NULL, append = TRUE,
                     check = TRUE, output = TRUE) {

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
    var.names <- .var.names(..., data = data, group = group, split = split, check.chr = "a numeric vector, matrix or data frame")

    # Extract data
    x <- data[, var.names]

    # Grouping variable
    if (isTRUE(!is.null(group))) { group <- data[, group] }

    # Splitting variable
    if (isTRUE(!is.null(split))) { split <- data[, split] }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Data and cluster
    var.group <- .var.group(data = x, group = group, split = split)

    if (isTRUE(!is.null(var.group$data)))  { x <- var.group$data }
    if (isTRUE(!is.null(var.group$group))) { group <- var.group$group }
    if (isTRUE(!is.null(var.group$split))) { split <- var.group$split }

  }

  # Convert 'x' into a vector when only one variable specified in 'x'
  if (isTRUE(ncol(data.frame(x)) == 1L)) { x <- unlist(x, use.names = FALSE) }

  # Check 'group'
  if (isTRUE(!is.null(group))) {

    if (nrow(data.frame(group)) != nrow(data.frame(x))) { stop("Length of the vector or factor specified in the argument 'group' does not match with 'x'.", call. = FALSE) }

    # Convert 'group' into a vector
    group <- unlist(group, use.names = FALSE)

  }

  # Check 'split'
  if (isTRUE(!is.null(split))) {

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

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

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

      warning(paste0("Listwise deletion of incomplete data, number of cases removed from the analysis: ", length(attributes(x.split)$na.action)), call. = FALSE)

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

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

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

    object.group <- lapply(split(x, f = group), function(y) misty::descript(y, data = NULL, group = NULL, split = NULL, sort.var = sort.var, na.omit = FALSE,
                                                                            as.na = NULL, check = FALSE, output = FALSE)$result)

    result <- data.frame(group = rep(names(object.group), each = ncol(x)),
                         eval(parse(text = paste0("rbind(", paste0("object.group[[", seq_len(length(object.group)), "]]", collapse = ", "), ")"))),
                         stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## No Grouping, Split ####
  } else if (isTRUE(is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, stringsAsFactors = FALSE), f = split),
                     function(y) misty::descript(y, data = NULL, group = NULL, split = NULL, sort.var = sort.var, na.omit = FALSE,
                                                 as.na = NULL, check = FALSE, output = FALSE)$result)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Grouping, Split ####
  } else if (isTRUE(!is.null(group) && !is.null(split))) {

    result <- lapply(split(data.frame(x, group = group, stringsAsFactors = FALSE), f = split),
                     function(y) misty::descript(y[, -grep("group", names(y))], data = NULL, group = y$group, split = NULL, sort.var = sort.var, na.omit = FALSE,
                                                 as.na = NULL, check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "descript",
                 data = list(x = x, group = group, split = split),
                 args = list(print = print, sort.var = sort.var, na.omit = na.omit,
                             digits = digits, as.na = as.na, write = write, append = append,
                             check = check, output = output),
                 result = result)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Write Results --------------------------------------------------------------

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
