#' Proportion of Missing Data for Each Case
#'
#' This function computes the proportion of missing data for each case in a matrix
#' or data frame.
#'
#' @param x           a matrix or data frame.
#' @param digits      an integer value indicating the number of decimal places to be
#'                    used for displaying proportions.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
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
#' dat <- data.frame(x = c(1, NA, NA, 6, 3),
#'                   y = c(7, NA, 8, 9, NA),
#'                   z = c(2, NA, 3, NA, 5))
#'
#' # Compute proportion of missing data (\code{NA}) for each case in the data frame
#' na.prop(dat)
na.prop <- function(x, digits = 2, as.na = NULL, check = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE) }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #------------------------------------------

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    #......
    # Matrix or data frame for the argument 'x'?
    if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE) }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L | digits < 0L)) { stop("Specify a positive integer value for the argument 'digits'", call. = FALSE) }

  }

  ####################################################################################
  # Data

  #------------------------------------
  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na)

  }

  ####################################################################################
  # Main Function

  object <- round(rowMeans(is.na(x)), digits = digits)

  ####################################################################################
  # Output

  return(object)

}
