#' Data Frame Sorting
#'
#' This function arranges a data frame in increasing or decreasing order according to one or more variables.
#'
#' @param x           a data frame.
#' @param ...         a sorting variable or a sequence of sorting variables which are specified without
#'                    quotes \code{''} or double quotes \code{""}.
#' @param decreasing  logical: if \code{TRUE}, the sort is decreasing.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{df.merge}}, \code{\link{df.rename}}, \code{\link{df.rename}}
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}. Wadsworth & Brooks/Cole.
#'
#' Knuth, D. E. (1998) \emph{The Art of Computer Programming, Volume 3: Sorting and Searching} (2nd ed.). Addison-Wesley.
#'
#' @return
#' Returns data frame \code{x} sorted according to the variables specified in \code{...}, a matrix will
#' be coerced to a data frame.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(5, 2, 5, 5, 7, 2),
#'                   y = c(1, 6, 2, 3, 2, 3),
#'                   z = c(2, 1, 6, 3, 7, 4))
#'
#' # Sort data frame 'dat' by "x" in increasing order
#' df.sort(dat, x)
#'
#' # Sort data frame 'dat' by "x" in decreasing order
#' df.sort(dat, x, decreasing = TRUE)
#'
#' # Sort data frame 'dat' by "x" and "y" in increasing order
#' df.sort(dat, x, y)
#'
#' # Sort data frame 'dat' by "x" and "y" in decreasing order
#' df.sort(dat, x, y, decreasing = TRUE)
df.sort <- function(x, ..., decreasing = FALSE, check = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a data frame for the argument 'x'", call. = FALSE)

  }

  #.........................
  # Data frame for the argument 'x'?
  if (!is.data.frame(x)) {

    stop("Please specifiy a data frame for the argument 'x'.", call. = FALSE)

  }


  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #.........................................

  if (isTRUE(check)) {

    #.........................
    # Check input 'decreasing'
    if (isFALSE(isTRUE(decreasing) | isFALSE(decreasing))) {

      stop("Please specify TRUE or FALSE for the argument 'decreasing'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  x.order <- eval(substitute(order(..., decreasing = decreasing)), envir = x, enclos = parent.frame())

  if (length(x.order) != nrow(x)) {

    stop("Length of ordering vectors do not match the number of rows in 'x'", call. = FALSE)

  }

  object <- x[x.order, , drop = FALSE]
  row.names(object) <- NULL

  ####################################################################################
  # Return object

  return(object)

}