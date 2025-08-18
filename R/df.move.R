#' Move Variable(s) in a Data Frame
#'
#' This function moves variables to a different position in the data frame, i.e.,
#' changes the column positions in the data frame. By default, variables specified
#' in the first argument \code{...} are moved to the first position in the data
#' frame specified in the argument \code{data}.
#'
#' @param data   a data frame.
#' @param ...    an expression indicating the variable names in \code{data} to
#'               move. Note that the operators \code{+}, \code{-}, \code{~},
#'               \code{:}, \code{::}, and \code{!} can also be used to select
#'               variables, see Details in the \code{\link{df.subset}} function.
#' @param before a character string indicating a variable in \code{data}.
#'               Variable(s) specified in \code{...} are moved to the left-hand
#'               side of this variable.
#' @param after  a character string indicating a variable in \code{data}.
#'               Variable(s) specified in \code{...} are moved to the right-hand
#'               side of this variable.
#' @param first  logical: if \code{TRUE} (default), variable(s) specified in
#'               \code{...} will be moved to the first position in 'data', if
#'               \code{FALSE}, variable(s) specified in \code{...} will be moved
#'               to the last position in 'data'.
#' @param check  logical: if \code{TRUE} (default), argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{df.check}}, \code{\link{df.duplicated}}, \code{\link{df.unique}},
#' \code{\link{df.head}}, \code{\link{df.tail}}, \code{\link{df.long}},
#' \code{\link{df.wide}}, \code{\link{df.merge}}, \code{\link{df.rbind}},
#' \code{\link{df.rename}}, \code{\link{df.sort}}, \code{\link{df.subset}}
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}.
#' Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns the data frame in \code{data} with columns in a different place.
#'
#' @export
#'
#' @examples
#' # Example 1: Move variables 'hp' and 'am' to the first position
#' df.move(mtcars, hp, am)
#'
#' # Example 2: Move variables 'hp' and 'am' to the last position
#' df.move(mtcars, hp, am, first = FALSE)
#'
#' # Example 3: Move variables 'hp' and 'am' to the left-hand side of 'disp'
#' df.move(mtcars, hp, am, before = "disp")
#'
#' # Example 4: Move variables 'hp' and 'am' to the right-hand side of 'disp'
#' df.move(mtcars, hp, am, after = "disp")
df.move <- function(data, ..., before = NULL, after = NULL, first = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify the argument 'data'.", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Variable names
  var.names <- .var.names(data = data, ...)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = "first", envir = environment(), input.check = check)

  if (isTRUE(check)) {

    # Check input 'before'
    if (isTRUE(!is.null(before))) {

      if (isTRUE(!is.character(before) || length(before) != 1L)) { stop("Please specify a character string for the argument 'before", call. = FALSE) }

      if (isTRUE(!before %in% colnames(data))) { stop("Variable specified in 'before' was not found in the data frame specified in 'data.", call. = FALSE) }

      if (isTRUE(before %in% var.names)) { stop("Variables to move specified in '...' cannot be specified in 'before'.", call. = FALSE) }

    }

    # Check input 'after'
    if (isTRUE(!is.null(after))) {

      if (isTRUE(!is.character(after) || length(after) != 1L)) { stop("Please specify a character string for the argument 'after", call. = FALSE) }

      if (isTRUE(!after %in% colnames(data))) { stop("Variable specified in 'after' was not found in the data frame specified in 'data.", call. = FALSE) }

      if (isTRUE(after %in% var.names)) { stop("Variables to move specified in '...' cannot be specified in 'after'.", call. = FALSE) }

    }

    # Check input 'before' and 'after'
    if (!is.null(before) && !is.null(after)) { stop("Please specify the argument 'before' or 'after', but not both arguments.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Indices of 'var.names'
  var.names.ind <- NULL
  for (i in var.names) { var.names.ind <- c(var.names.ind, which(colnames(data) %in% i)) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'before' and 'after' are NULL ####

  #...................
  ### Move variables to the first position in 'data' ####
  if (isTRUE(first)) {

    object <- data[, c(var.names.ind, which(!colnames(data) %in% var.names))]

  ### Move variables to the last position in 'data' ####
  } else {

    object <- data[, c(which(!colnames(data) %in% var.names), var.names.ind)]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument 'before' and 'after' are not NULL ####

  # Move variables before variables specified in 'before'
  if (isTRUE(!is.null(before) & is.null(after))) {

    # Indices of 'data'
    pos.data <- seq_len(ncol(data))

    # Index of 'before'
    pos.before <- which(colnames(data) %in% before)

    # Indices of 'data' without indices of 'var.names'
    pos.setdiff <- setdiff(pos.data, var.names.ind)

    object <- data[, c(pos.setdiff[which(pos.setdiff <= (pos.before - 1L))], var.names.ind, pos.setdiff[which(pos.setdiff >= pos.before)])]

  # Move variables after variables specified in 'after'
  } else if (isTRUE(is.null(before) & !is.null(after))) {

    # Indices of 'data'
    pos.data <- seq_len(ncol(data))

    # Index of 'after'
    pos.after <- which(colnames(data) %in% after)

    # Indices of 'data' without indices of 'var.names'
    pos.setdiff <- setdiff(pos.data, var.names.ind)

    object <- data[, c(pos.setdiff[which(pos.setdiff <= pos.after)], var.names.ind, pos.setdiff[which(pos.setdiff >= (pos.after + 1L))])]

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
