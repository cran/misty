#' Extract Duplicated or Unique Rows
#'
#' This function extracts duplicated or unique rows from a matrix or data frame.
#'
#' Note that \code{df.unique(x)} is equivalent to \code{unique(x)}. That is, the
#' main difference between the \code{df.unique()} and the \code{unique()} function is
#' that the \code{df.unique()} function provides the \code{...} argument to
#' specify a variable or multiple variables which are used to determine unique rows.
#'
#' @param x              a matrix or data frame.
#' @param ...            a variable or multiple variables which are specified without
#'                       quotes \code{''} or double quotes \code{""} used to determine
#'                       duplicated or unique rows. By default, all variables in \code{x}
#'                       are used.
#' @param first          logical: if \code{TRUE}, the \code{df.duplicated()} function will return
#'                       duplicated rows including the first of identical rows.
#' @param keep.all       logical: if \code{TRUE}, the function will return all variables in \code{x}
#'                       after extracting duplicated or unique rows based on the variables specified
#'                       in the argument \code{...}.
#' @param from.last      logical: if \code{TRUE}, duplication will be considered from the reversed
#'                       side, i.e., the last of identical rows would correspond to \code{duplicated = FALSE}.
#'                       Note that this argument is only used when \code{first = FALSE}.
#' @param keep.row.names logical: if \code{TRUE}, the row names from \code{x} are kept, otherwise they
#'                       are set to \code{NULL}.
#' @param check          logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @name df.duplicated
#'
#' @seealso
#' \code{\link{df.unique}}, \code{\link{df.merge}}, \code{\link{df.rbind}},
#' \code{\link{df.rename}}, \code{\link{df.sort}}
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}. Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns duplicated or unique rows of the matrix or data frame in \code{x}.
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
#' #--------------------------------------
#' # df.duplicated() function
#'
#' # Extract duplicated rows based on all variables
#' df.duplicated(dat)
#'
#' # Extract duplicated rows based on x4
#' df.duplicated(dat, x4)
#'
#' # Extract duplicated rows based on x2 and x3
#' df.duplicated(dat, x2, x3)
#'
#' # Extract duplicated rows based on all variables
#' # exclude first of identical rows
#' df.duplicated(dat, first = FALSE)
#'
#' # Extract duplicated rows based on x2 and x3
#' # do not return all variables
#' df.duplicated(dat, x2, x3, keep.all = FALSE)
#'
#' # Extract duplicated rows based on x4
#' # consider duplication from the reversed side
#' df.duplicated(dat, x4, first = FALSE, from.last = TRUE)
#'
#' # Extract duplicated rows based on x2 and x3
#' # set row names to NULL
#' df.duplicated(dat, x2, x3, keep.row.names = FALSE)
#'
#' #--------------------------------------
#' # df.unique() function
#'
#' # Extract unique rows based on all variables
#' unique(dat)
#'
#' # Extract unique rows based on x4
#' df.unique(dat, x4)
#'
#' # Extract unique rows based on x1, x2, and x3
#' df.unique(dat, x1, x2, x3)
#'
#' # Extract unique rows based on x2 and x3
#' # do not return all variables
#' df.unique(dat, x2, x3, keep.all = FALSE)
#'
#' # Extract unique rows based on x4
#' # consider duplication from the reversed side
#' df.unique(dat, x4, from.last = TRUE)
#'
#' # Extract unique rows based on x2 and x3
#' # set row names to NULL
#' df.unique(dat, x2, x3, keep.row.names = FALSE)
df.duplicated <- function(x, ..., first = TRUE, keep.all = TRUE, from.last = FALSE,
                          keep.row.names = TRUE, check = TRUE) {

  ####################################################################################
  # Data

  # Variables specified in ...
  var.names <- misty::chr.omit(sapply(substitute(list(...)), as.character), omit = "list")

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE)

  }

  # No variables specified in ..., i.e., use all variables in x
  if (isTRUE(length(var.names) == 0L)) { var.names <- colnames(x) }

  #......
  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input '...'
  var.names.check <- !var.names %in% colnames(x)
  if (isTRUE(any(var.names.check))) {

    stop(paste0("Variables specified in '...' were not all found in 'x': ",
                paste0(var.names[var.names.check], collapse = ", ")), call. = FALSE)

  }

  #.............
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #.............
    # Check input 'first'
    if (isTRUE(!is.logical(first))) {

      stop("Please specify TRUE or FALSE for the argument 'first'.", call. = FALSE)

    }

    #.............
    # Check input 'keep.all'
    if (isTRUE(!is.logical(keep.all))) {

      stop("Please specify TRUE or FALSE for the argument 'keep.all'.", call. = FALSE)

    }

    #.............
    # Check input 'from.last'
    if (isTRUE(!is.logical(from.last))) {

      stop("Please specify TRUE or FALSE for the argument 'from.last'.", call. = FALSE)

    }

    #.............
    # Check input 'keep.row.names'
    if (isTRUE(!is.logical(keep.row.names))) {

      stop("Please specify TRUE or FALSE for the argument 'keep.row.names'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  # Duplicated rows including the first of identical rows
  if (isTRUE(first)) {

    # Return all variables in x
    if (isTRUE(keep.all)) {

      object <- x[duplicated(x[, var.names], fromLast = FALSE) |
                  duplicated(x[, var.names], fromLast = TRUE), , drop = FALSE]

    # Return variables in ...
    } else {

      object <- x[duplicated(x[, var.names], fromLast = FALSE) |
                  duplicated(x[, var.names], fromLast = TRUE), var.names, drop = FALSE]

    }

  # Duplicated rows excluding the first of identical rows
  } else {

    # Return all variables in x
    if (isTRUE(keep.all)) {

      object <- x[duplicated(x[, var.names], fromLast = from.last), , drop = FALSE]

      # Return variables in ...
    } else {

      object <- x[duplicated(x[, var.names], fromLast = from.last), var.names, drop = FALSE]

    }

  }

  # Remove row names
  if (!isTRUE(keep.row.names)) {

    row.names(object) <- NULL

  }

  ####################################################################################
  # Return object

  if (isTRUE(nrow(object) == 0L)) {

    warning("No duplicated rows found in the matrix or data frame specified in 'x'.",
            call. = FALSE)

  }

  return(object)

}

#' @rdname df.duplicated
df.unique <- function(x, ..., keep.all = TRUE, from.last = FALSE, keep.row.names = TRUE,
                      check = TRUE) {

  ####################################################################################
  # Data

  # Variables specified in ...
  var.names <- misty::chr.omit(sapply(substitute(list(...)), as.character), omit = "list")

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE)

  }

  # No variables specified in  ..., i.e., use all variables in x
  if (isTRUE(length(var.names) == 0L)) { var.names <- colnames(x) }

  #......
  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input '...'
  var.names.check <- !var.names %in% colnames(x)
  if (isTRUE(any(var.names.check))) {

    stop(paste0("Variables specified in ... were not all found in 'x': ",
                paste0(var.names[var.names.check], collapse = ", ")), call. = FALSE)

  }

  #.............
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #.............
    # Check input 'keep.all'
    if (isTRUE(!is.logical(keep.all))) {

      stop("Please specify TRUE or FALSE for the argument 'keep.all'.", call. = FALSE)

    }

    #.............
    # Check input 'from.last'
    if (isTRUE(!is.logical(from.last))) {

      stop("Please specify TRUE or FALSE for the argument 'from.last'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  # Return all variables in x
  if (isTRUE(keep.all)) {

    object <- x[!duplicated(x[, var.names], fromLast = from.last), , drop = FALSE]

  # Return variables in ...
  } else {

    object <- x[!duplicated(x[, var.names], fromLast = from.last), var.names, drop = FALSE]

  }

  # Remove row names
  if (!isTRUE(keep.row.names)) {

    row.names(object) <- NULL

  }

  ####################################################################################
  # Return object

  return(object)

}
