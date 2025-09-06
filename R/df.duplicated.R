#' Extract Duplicated or Unique Rows
#'
#' The function \code{df.duplicated} extracts duplicated rows and the function
#' \code{df.unique} extracts unique rows from a matrix or data frame.
#'
#' @param data           a data frame.
#' @param ...            an expression indicating the variable names in \code{data}
#'                       used to determine duplicated or unique rows.e.g.,
#'                       \code{df.duplicated(x1, x2, data = dat)}. Note that the
#'                       operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                       \code{::}, and \code{!} can also be used to select
#'                       variables, see Details in the \code{\link{df.subset}}
#'                       function.
#' @param first          logical: if \code{TRUE} (default), the \code{df.duplicated()}
#'                       function will return duplicated rows including the first
#'                       of identical rows.
#' @param keep.all       logical: if \code{TRUE} (default), the function will
#'                       return all variables in \code{data} after extracting
#'                       duplicated or unique rows based on the variables specified
#'                       in the argument \code{...}.
#' @param from.last      logical: if \code{TRUE}, duplication will be considered
#'                       from the reversed side, i.e., the last of identical rows
#'                       would correspond to \code{duplicated = FALSE}.
#'                       Note that this argument is only used when \code{first = FALSE}.
#' @param keep.row.names logical: if \code{TRUE} (default), the row names from \code{data}
#'                       are kept, otherwise they are set to \code{NULL}.
#' @param check          logical: if \code{TRUE} (default), argument specification
#'                       is checked.
#'
#' @details
#' Note that \code{df.unique(x)} is equivalent to \code{unique(x)}. That is, the
#' main difference between the \code{df.unique()} and the \code{unique()} function
#' is that the \code{df.unique()} function provides the \code{...} argument to
#' specify a variable or multiple variables which are used to determine unique
#' rows.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @name df.duplicated
#'
#' @seealso
#' \code{\link{df.check}}, \code{\link{df.head}}, \code{\link{df.tail}},
#' \code{\link{df.long}}, \code{\link{df.wide}}, \code{\link{df.merge}},
#' \code{\link{df.move}}, \code{\link{df.rbind}}, \code{\link{df.rename}},
#' \code{\link{df.sort}}, \code{\link{df.subset}}
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
#' dat <- data.frame(x1 = c(1, 1, 2, 1, 4), x2 = c(1, 1, 2, 1, 6), x3 = c(2, 2, 3, 2, 6),
#'                   x4 = c(1, 1, 2, 2, 4), x5 = c(1, 1, 4, 4, 3))
#'
#' #----------------------------------------------------------------------------
#' # df.duplicated() function
#'
#' # Example 1: Extract duplicated rows based on all variables
#' df.duplicated(dat)
#'
#' # Example 2: Extract duplicated rows based on 'x4'
#' df.duplicated(dat, x4)
#'
#' # Example 3: Extract duplicated rows based on 'x2' and 'x3'
#' df.duplicated(dat, x2, x3)
#'
#' # Example 4: Extract duplicated rows based on all variables
#' # exclude first of identical rows
#' df.duplicated(dat, first = FALSE)
#'
#' # Example 5: Extract duplicated rows based on 'x2' and 'x3'
#' # do not return all variables
#' df.duplicated(dat, x2, x3, keep.all = FALSE)
#'
#' # Example 6: Extract duplicated rows based on 'x4'
#' # consider duplication from the reversed side
#' df.duplicated(dat, x4, first = FALSE, from.last = TRUE)
#'
#' # Example 7: Extract duplicated rows based on 'x2' and 'x3'
#' # set row names to NULL
#' df.duplicated(dat, x2, x3, keep.row.names = FALSE)
#'
#' #----------------------------------------------------------------------------
#' # df.unique() function
#'
#' # Example 8: Extract unique rows based on all variables
#' df.unique(dat)
#'
#' # Example 9: Extract unique rows based on 'x4'
#' df.unique(dat, x4)
#'
#' # Example 10: Extract unique rows based on 'x1', 'x2', and 'x3'
#' df.unique(dat, x1, x2, x3)
#'
#' # Example 11: Extract unique rows based on 'x2' and 'x3'
#' # do not return all variables
#' df.unique(dat, x2, x3, keep.all = FALSE)
#'
#' # Example 12: Extract unique rows based on 'x4'
#' # consider duplication from the reversed side
#' df.unique(dat, x4, from.last = TRUE)
#'
#' # Example 13: Extract unique rows based on 'x2' and 'x3'
#' # set row names to NULL
#' df.unique(dat, x2, x3, keep.row.names = FALSE)
df.duplicated <- function(data, ..., first = TRUE, keep.all = TRUE,
                          from.last = FALSE, keep.row.names = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a data frame for the argument 'data'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Variable names
  if (isTRUE(missing(...))) {

    var.names <- colnames(data)

  } else {

    var.names <- .var.names(..., data = data)

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("first", "keep.all", "from.last", "keep.row.names"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Duplicated rows including the first of identical rows
  if (isTRUE(first)) {

    # Return all variables in data
    if (isTRUE(keep.all)) {

      object <- data[duplicated(data[, var.names], fromLast = FALSE) | duplicated(data[, var.names], fromLast = TRUE), , drop = FALSE]

    # Return variables in ...
    } else {

      object <- data[duplicated(data[, var.names], fromLast = FALSE) | duplicated(data[, var.names], fromLast = TRUE), var.names, drop = FALSE]

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

  # No duplicated rows
  if (isTRUE(nrow(object) == 0L)) { warning("No duplicated rows found in the data frame specified in 'data'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#' @rdname df.duplicated
df.unique <- function(data, ..., keep.all = TRUE, from.last = FALSE,
                      keep.row.names = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a numeric vector for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Variable names
  if (isTRUE(missing(...))) {

    var.names <- colnames(data)

  } else {

    var.names <- .var.names(..., data = data)

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("keep.all", "from.last", "keep.row.names"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Return all variables in data
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

#_______________________________________________________________________________
