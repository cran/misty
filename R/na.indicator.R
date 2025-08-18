#' Missing Data Indicator Matrix
#'
#' This function creates a missing data indicator matrix \eqn{R} that denotes
#' whether values are observed or missing, i.e., \eqn{r = 0} if a value is
#' observed, and \eqn{r = 1} if a value is missing.
#'
#' @param data   a data frame with incomplete data, where missing
#'               values are coded as \code{NA}.
#' @param ...    an expression indicating the variable names in \code{data}, e.g.,
#'               \code{na.indicator(dat, x1, x2, x3)}. Note that the operators
#'               \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'               and \code{!} can also be used to select variables, see 'Details'
#'               in the \code{\link{df.subset}} function.
#' @param na     an integer value specifying the value representing missing values,
#'               i.e., either \code{na = 0} for \code{0 = missing} and \code{1 = observed},
#'               or \code{na = 1} (default) for \code{0} (observed) and \code{1 = missing}.
#' @param append logical: if \code{TRUE} (default), missing data indicator matrix
#'               is appended to the data frame specified in the argument \code{data}.
#' @param name   a character string indicating the  name suffix of indicator variables
#'               By default, the indicator variables are named with the ending
#'               \code{".i"} resulting in e.g. \code{"x1.i"} and \code{"x2.i"}.
#'               Note that when selecting one single variable, the indicator variable
#'               is named \code{x.i} by default or named after the argument \code{name}.
#' @param as.na  a numeric vector indicating user-defined missing values,
#'               i.e. these values are converted to \code{NA} before conducting
#'               the analysis.
#' @param check  logical: if \code{TRUE} (default), argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.auxiliary}},
#' \code{\link{na.coverage}}, \code{\link{na.descript}}, \code{\link{na.pattern}},
#' \code{\link{na.prop}}, \code{\link{na.test}}
#'
#' @references
#' Enders, C. K. (2022). \emph{Applied missing data analysis} (2nd ed.). The Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576.
#' https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.). Chapman & Hall.
#'
#' @return
#' Returns a data frame with \eqn{r = 1} if a value is observed, and \eqn{r = 0}
#' if a value is missing.
#'
#' @export
#'
#' @examples
#' # Example 1: Create missing data indicator matrix
#' na.indicator(airquality)
#'
#' # Example 2: Do not append missing data indicator matrix to the data frame
#' na.indicator(airquality, append = FALSE)
na.indicator <- function(data, ..., na = 1, append = TRUE, name = ".i",
                         as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- as.data.frame(data[, .var.names(data = data, ...), drop = FALSE])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Data frame
    x <- as.data.frame(data)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = "append", numeric = list(na = 1L), character = list(name = 1L), envir = environment(), input.check = check)

  # Additional check
  if (isTRUE(check)) {

    # Check input 'na'
    if (isTRUE(na != 0L && na != 1L)) { stop("Please specify 0 or 1 for the argument 'na'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert NA ####

  if (isTRUE(na == 1L)) {

    object <- data.frame(apply(x, 2L, function(y) as.numeric(is.na(y))), row.names = rownames(x))

  } else {

    object <- data.frame(apply(x, 2L, function(y) as.numeric(!is.na(y))), row.names = rownames(x))

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## One Column ####

  if (isTRUE(ncol(object) == 1L && grepl("......", names(object)))) { object <- setNames(object, nm = "x") }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!is.null(data) && append)) { object <- data.frame(data, setNames(as.data.frame(object), nm = paste0(colnames(object), name))) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
