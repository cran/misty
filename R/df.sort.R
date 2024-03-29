#' Data Frame Sorting
#'
#' This function arranges a data frame in increasing or decreasing order according
#' to one or more variables.
#'
#' @param x           a data frame.
#' @param ...         a sorting variable or a sequence of sorting variables which
#'                    are specified without quotes \code{''} or double quotes \code{""}.
#' @param decreasing  logical: if \code{TRUE}, the sort is decreasing.
#' @param check       logical: if \code{TRUE} (default), argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{df.duplicated}}, \code{\link{df.merge}}, \code{\link{df.move}},
#' \code{\link{df.rbind}}, \code{\link{df.rename}}, \code{\link{df.subset}}
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}.
#' Wadsworth & Brooks/Cole.
#'
#' Knuth, D. E. (1998) \emph{The Art of Computer Programming, Volume 3: Sorting
#' and Searching} (2nd ed.). Addison-Wesley.
#'
#' @return
#' Returns data frame \code{x} sorted according to the variables specified in
#' \code{...}, a matrix will be coerced to a data frame.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(5, 2, 5, 5, 7, 2),
#'                   y = c(1, 6, 2, 3, 2, 3),
#'                   z = c(2, 1, 6, 3, 7, 4))
#'
#' # Example 1: Sort data frame 'dat' by "x" in increasing order
#' df.sort(dat, x)
#'
#' # Example 2: Sort data frame 'dat' by "x" in decreasing order
#' df.sort(dat, x, decreasing = TRUE)
#'
#' # Example 3: Sort data frame 'dat' by "x" and "y" in increasing order
#' df.sort(dat, x, y)
#'
#' # Example 4: Sort data frame 'dat' by "x" and "y" in decreasing order
#' df.sort(dat, x, y, decreasing = TRUE)
df.sort <- function(x, ..., decreasing = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Variables specified in ...
  var.names <- misty::chr.omit(sapply(substitute(list(...)), as.character), omit = "list")

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a data frame for the argument 'x'.", call. = FALSE) }

  # No variables specified in ..., i.e., use all variables in x
  if (isTRUE(length(var.names) == 0)) { var.names <- colnames(x) }

  # Data frame for the argument 'x'?
  if (isTRUE(!is.data.frame(x))) { stop("Please specify a data frame for the argument 'x'.", call. = FALSE) }

  # Check input '...'
  var.names.check <- !var.names %in% colnames(x)
  if (isTRUE(any(var.names.check))) {

    stop(paste0("Variables specified in '...' were not all found in 'x': ", paste0(var.names[var.names.check], collapse = ", ")), call. = FALSE)

  }

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  if (isTRUE(check)) {

    # Check input 'decreasing'
    if (isTRUE(!is.logical(decreasing))) { stop("Please specify TRUE or FALSE for the argument 'decreasing'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  x.order <- eval(substitute(order(..., decreasing = decreasing)), envir = x, enclos = parent.frame())

  if (isTRUE(length(x.order) != nrow(x))) {

    stop("Length of ordering vectors does not match with the number of rows in 'x'.", call. = FALSE)

  }

  object <- x[x.order, , drop = FALSE]
  row.names(object) <- NULL

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
