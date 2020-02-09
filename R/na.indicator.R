#' Missing Data Indicator Matrix
#'
#' This function creates a missing data indicator matrix \eqn{R} that denotes whether values
#' are observed or missing, i.e., \eqn{r = 1} if a value is observed, and \eqn{r = 0} if a
#' value is missing.
#'
#' @param x           a matrix or data frame.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.auxiliary}}, \code{\link{na.coverage}},
#' \code{\link{na.descript}}, \code{\link{na.pattern}}, \code{\link{na.prop}}.
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576. https://doi.org/10.1146/annurev.psych.58.110405.085530
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
#' dat <- data.frame(x = c(1, NA, NA, 6, 3),
#'                   y = c(7, NA, 8, 9, NA),
#'                   z = c(2, NA, 3, NA, 5))
#'
#' # Create missing data indicator matrix \eqn{R}
#' na.indicator(dat)
na.indicator <- function(x, as.na = NULL, check = TRUE) {

  ####################################################################################
  # Input Check

  #----------------------------------------
  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'.",
         call. = FALSE)

  }

  if (isTRUE(check)) {

  }

  ####################################################################################
  # Data

  #-----------------------------------------------------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x <- misty::as.na(x, na = as.na, check = check)

  }

  #-----------------------------------------------------------------------------------
  # Main Function

  object <- apply(x, 2, function(y) as.numeric(!is.na(y)))

  if (is.data.frame(x)) {

    object  <- as.data.frame(object)
    row.names(object) <- rownames(x)

  }

  #-----------------------------------------------------------------------------------
  # Return object

  return(object)

}
