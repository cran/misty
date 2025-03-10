#' Variance-Covariance Coverage
#'
#' This function computes the proportion of cases that contributes for the calculation
#' of each variance and covariance.
#'
#' @param data   a data frame with incomplete data, where missing
#'               values are coded as \code{NA}.
#' @param ...    an expression indicating the variable names in \code{data}, e.g.,
#'               \code{na.coverage(dat, x1, x2, x3)}. Note that the operators
#'               \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'               and \code{!} can also be used to select variables, see 'Details'
#'               in the \code{\link{df.subset}} function.
#' @param tri    a character string or character vector indicating which triangular
#'               of the matrix to show on the console, i.e., \code{both} for
#'               upper and lower triangular, \code{lower} (default) for the
#'               lower triangular, and \code{upper} for the upper triangular.
#' @param digits an integer value indicating the number of decimal places to
#'               be used for displaying proportions.
#' @param as.na  a numeric vector indicating user-defined missing values,
#'               i.e. these values are converted to \code{NA} before conducting
#'               the analysis.
#' @param write  a character string naming a file for writing the output into
#'               either a text file with file extension \code{".txt"} (e.g.,
#'               \code{"Output.txt"}) or Excel file with file extension
#'               \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'               name does not contain any file extension, an Excel file will
#'               be written.
#' @param append logical: if \code{TRUE} (default), output will be appended
#'               to an existing text file with extension \code{.txt} specified
#'               in \code{write}, if \code{FALSE} existing text file will be
#'               overwritten.
#' @param check  logical: if \code{TRUE} (default), argument specification is checked.
#' @param output logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.auxiliary}},
#' \code{\link{na.descript}}, \code{\link{na.indicator}}, \code{\link{na.pattern}},
#' \code{\link{na.prop}}, \code{\link{na.test}}, \code{\link{write.result}}
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576. https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.).
#' Chapman & Hall.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{data frame used for the current analysis}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{result table}
#'
#' @export
#'
#' @examples
#' # Example 1: Compute variance-covariance coverage
#' na.coverage(airquality)
#'
#' \dontrun{
#' # Example 2a: Write Results into a text file
#' na.coverage(airquality, write = "Coverage.txt")
#'
#' # Example 2b: Write Results into a Excel file
#' na.coverage(airquality, write = "Coverage.xlsx")
#' }
na.coverage <- function(data, ..., tri = c("both", "lower", "upper"), digits = 2,
                        as.na = NULL, write = NULL, append = TRUE, check = TRUE,
                        output = TRUE) {

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
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Extract data and convert tibble into data frame or vector
    x <- as.data.frame(data[, .var.names(..., data = data), drop = FALSE])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

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
  .check.input(logical = c("append", "output"), s.character = list(tri = c("both", "lower", "upper")), args = c("digits", "write2"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print triangular ####

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Compute pairwise coverage
  cov.coverage <- data.frame(combn(ncol(x), m = 2L)) |> (\(y) vapply(y, function(z) nrow(na.omit(x[, c(z[1L], z[2L])])) / nrow(x), FUN.VALUE = double(1L)))()

  # Coverage matrix
  restab <- matrix(NA, ncol = ncol(x), nrow = (ncol(x)), dimnames = list(colnames(x), colnames(x)))

  # Assign coverage to lower triangular
  restab[lower.tri(restab)] <- cov.coverage

  # Copy lower triangular to upper triangular
  restab[upper.tri(restab)] <- t(restab)[upper.tri(restab)]

  # Variance coverage
  diag(restab) <- vapply(x, function(y) mean(!is.na(y)), FUN.VALUE = double(1L))

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "na.coverage",
                 data = x,
                 args = list(tri = tri, digits = digits, as.na = as.na, write = write, append = append, check = TRUE, output = output),
                 result = restab)

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
