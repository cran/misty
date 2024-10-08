#' Proportion of Missing Data for Each Case
#'
#' This function computes the proportion of missing data for each case in a matrix
#' or data frame.
#'
#' @param ...     a matrix or data frame with incomplete data, where missing
#'                values are coded as \code{NA}. Alternatively, an expression
#'                indicating the variable names in \code{data} e.g.,
#'                \code{na.prop(x1, x2, x3, data = dat)}. Note that the operators
#'                \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'                and \code{!} can also be used to select variables, see 'Details'
#'                in the \code{\link{df.subset}} function.
#' @param data    a data frame when specifying one or more variables in the
#'                argument \code{...}. Note that the argument is \code{NULL}
#'                when specifying a matrix or data frame for the argument \code{...}.
#' @param append  logical: if \code{TRUE} (default), variable with proportion of
#'                missing data is appended to the data frame specified in the
#'                argument \code{data}.
#' @param name    a character string indicating the name of the variable appended
#'                to the data frame specified in the argument \code{data} when
#'                \code{append = TRUE}. By default, the variable is named
#'                \code{na.prop}.
#' @param digits  an integer value indicating the number of decimal places to be
#'                used for displaying proportions.
#' @param as.na   a numeric vector indicating user-defined missing values,
#'                i.e. these values are converted to \code{NA} before conducting
#'                the analysis.
#' @param check   logical: if \code{TRUE} (default), argument specification is
#'                checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.auxiliary}},
#' \code{\link{na.coverage}}, \code{\link{na.descript}}, \code{\link{na.indicator}},
#' \code{\link{na.pattern}}, \code{\link{na.test}}
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576.
#' https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.).
#' Chapman & Hall.
#'
#' @return
#' Returns a numeric vector with the same length as the number of rows in \code{x}
#' containing the proportion of missing data.
#'
#' @export
#'
#' @examples
#' # Example 1a: Compute proportion of missing data for each case in the data frame
#' na.prop(airquality)
#'
#' # Example 1b: Alternative specification using the 'data' argument,
#' # append proportions to the data frame 'airquality'
#' na.prop(., data = airquality)
na.prop <- function(..., data = NULL, digits = 2, append = TRUE, name = "na.prop",
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
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Matrix or data frame for the argument 'x'?
    if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L | digits < 0L)) { stop("Specify a positive integer value for the argument 'digits'", call. = FALSE) }

    # Check input 'append'
    if (isTRUE(!is.logical(append))) { stop("Please specify TRUE or FALSE for the argument 'append'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  object <- round(rowMeans(is.na(x)), digits = digits)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!is.null(data) && append)) {

    object <- data.frame(data,  setNames(as.data.frame(object), nm = name))

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
