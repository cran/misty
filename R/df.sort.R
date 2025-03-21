#' Data Frame Sorting
#'
#' This function arranges a data frame in increasing or decreasing order according
#' to one or more variables.
#'
#' @param data        a data frame.
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
#' Returns data frame \code{data} sorted according to the variables specified in
#' \code{...}, a matrix will be coerced to a data frame.
#'
#' @export
#'
#' @examples
#' # Example 1: Sort data frame 'mtcars' by 'mpg' in increasing order
#' df.sort(mtcars, mpg)
#'
#' # Example 2: Sort data frame 'mtcars' by 'mpg' in decreasing order
#' df.sort(mtcars, mpg, decreasing = TRUE)
#'
#' # Example 3: Sort data frame 'mtcars' by 'mpg' and 'cyl' in increasing order
#' df.sort(mtcars, mpg, cyl)
#'
#' # Example 4: Sort data frame 'mtcars' by 'mpg' and 'cyl' in decreasing order
#' df.sort(mtcars, mpg, cyl, decreasing = TRUE)
df.sort <- function(data, ..., decreasing = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  # Variables specified in ...
  var.names <- misty::chr.omit(sapply(substitute(list(...)), as.character), omit = "list")

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  # No variables specified in ..., i.e., use all variables in data
  if (isTRUE(length(var.names) == 0)) { var.names <- colnames(data) }

  # Data frame for the argument 'data'?
  if (isTRUE(!is.data.frame(data))) { stop("Please specify a data frame for the argument 'data'.", call. = FALSE) }

  # Check input '...'
  (!var.names %in% colnames(data)) |> (\(y) if (isTRUE(any(y))) { stop(paste0("Variables specified in '...' were not all found in 'data': ", paste0(var.names[y], collapse = ", ")), call. = FALSE) })()

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input
  .check.input(logical = "decreasing", envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  object <- eval(substitute(order(..., decreasing = decreasing)), envir = data, enclos = parent.frame()) |>
    (\(y) if (isTRUE(length(y) != nrow(data))) {

      stop("Length of ordering vectors does not match with the number of rows in 'data'.", call. = FALSE)

    } else {

      return(data[y, , drop = FALSE])

    })()

  row.names(object) <- NULL

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
