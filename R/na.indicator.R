#' Missing Data Indicator Matrix
#'
#' This function creates a missing data indicator matrix \eqn{R} that denotes
#' whether values are observed or missing, i.e., \eqn{r = 0} if a value is
#' observed, and \eqn{r = 1} if a value is missing.
#'
#' @param ...    a matrix or data frame with incomplete data, where missing
#'               values are coded as \code{NA}. Alternatively, an expression
#'               indicating the variable names in \code{data} e.g.,
#'               \code{na.indicator(x1, x2, x3, data = dat)}. Note that the operators
#'               \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'               and \code{!} can also be used to select variables, see 'Details'
#'               in the \code{\link{df.subset}} function.
#' @param data   a data frame when specifying one or more variables in the
#'               argument \code{...}. Note that the argument is \code{NULL}
#'               when specifying a matrix or data frame for the argument \code{...}.
#' @param na     an integer value specifying, i.e., either \code{1} for
#'               \code{0 = observed} and \code{1 = missing}, or \code{0} (default)
#'               for \code{1 = observed} and \code{0 = missing}.
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
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576.
#' https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.). Chapman & Hall.
#'
#' @return
#' Returns a matrix or data frame with \eqn{r = 1} if a value is observed, and \eqn{r = 0}
#' if a value is missing.
#'
#' @export
#'
#' @examples
#' # Example 1a: Create missing data indicator matrix \eqn{R}
#' na.indicator(airquality)
#'
#' # Example 1b: Alternative specification using the 'data' argument
#' na.indicator(., data = airquality)
#'
#' # Example 2: Append missing data indicator matrix to the data frame
#' na.indicator(., data = airquality)
na.indicator <- function(..., data = NULL, na = 0, append = TRUE, name = ".i",
                         as.na = NULL, check = TRUE) {

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

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'na'
    if (isTRUE(na != 0L && na != 1L)) { stop("Please specify 0 or 1 for the argument 'na'.", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

    # Check input 'name'
    if (isTRUE(!is.character(name) || length(name) != 1L)) { stop("Please specify a character string for the argument 'name'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Convert NA
  if (isTRUE(na == 1L)) {

    object <- apply(x, 2L, function(y) as.numeric(is.na(y)))

  } else {

    object <- apply(x, 2L, function(y) as.numeric(!is.na(y)))

  }

  # As data frame
  if (isTRUE(is.data.frame(x))) {

    object  <- as.data.frame(object, stringsAsFactors = FALSE)
    row.names(object) <- rownames(x)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!is.null(data) && append)) {

    #...................
    ### Variable names ####

    if (isTRUE(ncol(object) == 1L)) {

      if (isTRUE(name == ".i")) {

        object <- setNames(as.data.frame(object), nm = "x.i")

      } else {

        object <- setNames(as.data.frame(object), nm = name)

      }

    } else {

      object <- setNames(as.data.frame(object), nm = paste0(colnames(object), name))

    }

    object <- data.frame(data, object)

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
