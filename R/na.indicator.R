#' Missing Data Indicator Matrix
#'
#' This function creates a missing data indicator matrix \eqn{R} that denotes
#' whether values are observed or missing, i.e., \eqn{r = 1} if a value is
#' observed, and \eqn{r = 0} if a value is missing.
#'
#' @param ...   a matrix or data frame with incomplete data, where missing
#'              values are coded as \code{NA}. Alternatively, an expression
#'              indicating the variable names in \code{data} e.g.,
#'              \code{na.indicator(x1, x2, x3, data = dat)}. Note that the operators
#'              \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'              and \code{!} can also be used to select variables, see 'Details'
#'              in the \code{\link{df.subset}} function.
#' @param data  a data frame when specifying one or more variables in the
#'              argument \code{...}. Note that the argument is \code{NULL}
#'              when specifying a matrix or data frame for the argument \code{...}.
#' @param as.na a numeric vector indicating user-defined missing values,
#'              i.e. these values are converted to \code{NA} before conducting
#'              the analysis.
#' @param check logical: if \code{TRUE} (default), argument specification is checked.
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
na.indicator <- function(..., data = NULL, as.na = NULL, check = TRUE) {

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
  # Main Function --------------------------------------------------------------

  object <- apply(x, 2L, function(y) as.numeric(!is.na(y)))

  if (isTRUE(is.data.frame(x))) {

    object  <- as.data.frame(object, stringsAsFactors = FALSE)
    row.names(object) <- rownames(x)

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
