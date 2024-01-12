#' Extract Duplicated or Unique Rows
#'
#' The function \code{df.duplicated} extracts duplicated rows and the function
#' \code{df.unique} extracts unique rows from a matrix or data frame.
#'
#' Note that \code{df.unique(x)} is equivalent to \code{unique(x)}. That is, the
#' main difference between the \code{df.unique()} and the \code{unique()} function
#' is that the \code{df.unique()} function provides the \code{...} argument to
#' specify a variable or multiple variables which are used to determine unique
#' rows.
#'
#' @param ...            an expression indicating the variable names in \code{data}
#'                       used to determine duplicated or unique rows.e.g.,
#'                       \code{df.duplicated(x1, x2, data = dat)}. Note that the
#'                       operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                       \code{::}, and \code{!} can also be used to select
#'                       variables, see Details in the \code{\link{df.subset}}
#'                       function.
#' @param data           a data frame.
#' @param first          logical: if \code{TRUE} (default), the \code{df.duplicated()}
#'                       function will return duplicated rows including the first
#'                       of identical rows.
#' @param keep.all       logical: if \code{TRUE} (default), the function will
#'                       return all variables in \code{x} after extracting
#'                       duplicated or unique rows based on the variables specified
#'                       in the argument \code{...}.
#' @param from.last      logical: if \code{TRUE}, duplication will be considered
#'                       from the reversed side, i.e., the last of identical rows
#'                       would correspond to \code{duplicated = FALSE}.
#'                       Note that this argument is only used when \code{first = FALSE}.
#' @param keep.row.names logical: if \code{TRUE} (default), the row names from \code{x}
#'                       are kept, otherwise they are set to \code{NULL}.
#' @param check          logical: if \code{TRUE} (default), argument specification
#'                       is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @name df.duplicated
#'
#' @seealso
#' \code{\link{df.merge}}, \code{\link{df.move}}, \code{\link{df.rbind}},
#' \code{\link{df.rename}}, \code{\link{df.sort}}, \code{\link{df.subset}}
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}.
#' Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns duplicated or unique rows of the data frame in \code{...} or \code{data}.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(1, 1, 2, 1, 4),
#'                   x2 = c(1, 1, 2, 1, 6),
#'                   x3 = c(2, 2, 3, 2, 6),
#'                   x4 = c(1, 1, 2, 2, 4),
#'                   x5 = c(1, 1, 4, 4, 3))
#'
#' #----------------------------------------------------------------------------
#' # df.duplicated() function
#'
#' # Example 1: Extract duplicated rows based on all variables
#' df.duplicated(., data = dat)
#'
#' # Example 2: Extract duplicated rows based on x4
#' df.duplicated(x4, data = dat)
#'
#' # Example 3: Extract duplicated rows based on x2 and x3
#' df.duplicated(x2, x3, data = dat)
#'
#' # Example 4: Extract duplicated rows based on all variables
#' # exclude first of identical rows
#' df.duplicated(., data = dat, first = FALSE)
#'
#' # Example 5: Extract duplicated rows based on x2 and x3
#' # do not return all variables
#' df.duplicated(x2, x3, data = dat, keep.all = FALSE)
#'
#' # Example 6: Extract duplicated rows based on x4
#' # consider duplication from the reversed side
#' df.duplicated(x4, data = dat, first = FALSE, from.last = TRUE)
#'
#' # Example 7: Extract duplicated rows based on x2 and x3
#' # set row names to NULL
#' df.duplicated(x2, x3, data = dat, keep.row.names = FALSE)
#'
#' #----------------------------------------------------------------------------
#' # df.unique() function
#'
#' # Example 8: Extract unique rows based on all variables
#' df.unique(., data = dat)
#'
#' # Example 9: Extract unique rows based on x4
#' df.unique(x4, data = dat)
#'
#' # Example 10: Extract unique rows based on x1, x2, and x3
#' df.unique(x1, x2, x3, data = dat)
#'
#' # Example 11: Extract unique rows based on x2 and x3
#' # do not return all variables
#' df.unique(x2, x3, data = dat, keep.all = FALSE)
#'
#' # Example 12: Extract unique rows based on x4
#' # consider duplication from the reversed side
#' df.unique(x4, data = dat, from.last = TRUE)
#'
#' # Example 13: Extract unique rows based on x2 and x3
#' # set row names to NULL
#' df.unique(x2, x3, data = dat, keep.row.names = FALSE)
df.duplicated <- function(..., data , first = TRUE, keep.all = TRUE,
                          from.last = FALSE, keep.row.names = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Variable names
  var.names <- .var.names(..., data = data, check.chr = "a matrix or data frame")

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'first'
    if (isTRUE(!is.logical(first))) { stop("Please specify TRUE or FALSE for the argument 'first'.", call. = FALSE) }

    # Check input 'keep.all'
    if (isTRUE(!is.logical(keep.all))) { stop("Please specify TRUE or FALSE for the argument 'keep.all'.", call. = FALSE) }

    # Check input 'from.last'
    if (isTRUE(!is.logical(from.last))) { stop("Please specify TRUE or FALSE for the argument 'from.last'.", call. = FALSE) }

    # Check input 'keep.row.names'
    if (isTRUE(!is.logical(keep.row.names))) { stop("Please specify TRUE or FALSE for the argument 'keep.row.names'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Duplicated rows including the first of identical rows
  if (isTRUE(first)) {

    # Return all variables in data
    if (isTRUE(keep.all)) {

      object <- data[duplicated(data[, var.names], fromLast = FALSE) |
                     duplicated(data[, var.names], fromLast = TRUE), , drop = FALSE]

    # Return variables in ...
    } else {

      object <- data[duplicated(data[, var.names], fromLast = FALSE) |
                     duplicated(data[, var.names], fromLast = TRUE), var.names, drop = FALSE]

    }

  # Duplicated rows excluding the first of identical rows
  } else {

    # Return all variables in data
    if (isTRUE(keep.all)) {

      object <- data[duplicated(data[, var.names], fromLast = from.last), , drop = FALSE]

    # Return variables in ...
    } else {

      object <- data[duplicated(data[, var.names], fromLast = from.last), var.names, drop = FALSE]

    }

  }

  # Remove row names
  if (!isTRUE(keep.row.names)) { row.names(object) <- NULL }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(nrow(object) == 0L)) {

    warning("No duplicated rows found in the data frame specified in 'data'.", call. = FALSE)

  }

  return(object)

}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#' @rdname df.duplicated
df.unique <- function(..., data = NULL, keep.all = TRUE, from.last = FALSE,
                      keep.row.names = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Variable names
  var.names <- .var.names(..., data = data, check.chr = "a matrix or data frame")

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'keep.all'
    if (isTRUE(!is.logical(keep.all))) { stop("Please specify TRUE or FALSE for the argument 'keep.all'.", call. = FALSE) }

    # Check input 'from.last'
    if (isTRUE(!is.logical(from.last))) { stop("Please specify TRUE or FALSE for the argument 'from.last'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Return all variables in x
  if (isTRUE(keep.all)) {

    object <- data[!duplicated(data[, var.names], fromLast = from.last), , drop = FALSE]

  # Return variables in ...
  } else {

    object <- data[!duplicated(data[, var.names], fromLast = from.last), var.names, drop = FALSE]

  }

  # Remove row names
  if (!isTRUE(keep.row.names)) { row.names(object) <- NULL }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
