#' Extract Unique Elements and Count Number of Unique Elements
#'
#' The function \code{uniq} returns a vector, list or data frame with duplicated
#' elements removed. By default, the function prints a data frame with missing
#' values omitted and unique elements sorted increasing. The function \code{uniq.n}
#' counts the number of unique elements in a vector or for each column in a matrix
#' or data frame. By default, missing values are omitted before counting the number
#' of unique elements.
#'
#' @param data       a vector, factor, matrix, or data frame.
#' @param ...        an expression indicating the variable names in \code{data},
#'                   e.g., \code{uniq(dat, x1, x2)} for selecting the variables
#'                   \code{x1} and \code{x2} from the data frame \code{dat}. Note
#'                   that the operators \code{+}, \code{-}, \code{~},
#'                   \code{:}, \code{::}, and \code{!} can also be used to select
#'                   variables, see 'Details' in the \code{\link{df.subset}} function.
#' @param na.rm      logical: if \code{TRUE} (default), missing values are omitted
#'                   before extracting unique elements. Note that missing values
#'                   are always omitted when writing the output into an Excel
#'                   file, i.e., \code{na.rm = TRUE}.
#' @param sort       logical: if \code{TRUE} (default), unique elements are sorted
#'                   after extracting unique elements.
#' @param decreasing logical: if \code{TRUE}, unique elements are sorted decreasing
#'                   when specifying \code{sort = TRUE}.
#' @param digits     an integer value indicating the number of decimal places to
#'                   be used when rounding numeric values before extracting unique
#'                   elements. By default, unique elements are extracted without
#'                   rounding, i.e., \code{digits = NULL}.
#' @param table      logical: if \code{TRUE} (default), unique elements are printed
#'                   in a data frame, if \code{FALSE} unique elements are printed
#'                   in a list. Note that unique elements are always printed in
#'                   a data frame when writing the output into an Excel file, i.e.
#'                   \code{table = TRUE}.
#' @param write      a character string naming a file for writing the output into
#'                   either a text file with file extension \code{".txt"} (e.g.,
#'                   \code{"Output.txt"}) or Excel file with file extension
#'                   \code{".xlsx"}  (e.g., \code{"Output.xlsx"}). If the file
#'                   name does not contain any file extension, an Excel file will
#'                   be written.
#' @param append     logical: if \code{TRUE} (default), output will be appended
#'                   to an existing text file with extension \code{.txt} specified
#'                   in \code{write}, if \code{FALSE} existing text file will be
#'                   overwritten.
#' @param check      logical: if \code{TRUE} (default), argument specification is checked.
#' @param output     logical: if \code{TRUE} (default), output is shown on the console.
#'
#' @details
#' The function \code{uniq} is a wrapper function in the form of \code{sort(unique(na.omit(x)))},
#' while the function \code{uniq.n} is a wrapper function in the form of \code{length(unique(na.omit(x)))}.
#'
#' @author
#' Takuya Yanagida
#'
#' @name uniq
#'
#' @seealso
#' \code{\link{df.duplicated}}, \code{\link{df.unique}}
#'
#' @references
#' Becker, R. A., Chambers, J. M., & Wilks, A. R. (1988). \emph{The New S Language}.
#' Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns an object of class \code{misty.object} when using the \code{uniq} function,
#' which is a list with following entries:
#'
#' \item{\code{call}}{function call}
#' \item{\code{type}}{type of analysis}
#' \item{\code{data}}{a vector, factor, matrix, or data frame}
#' \item{\code{args}}{specification of function arguments}
#' \item{\code{result}}{list with unique elements}
#'
#' or a vector with the count number of unique elements for a vector, factor or
#' each column in a matrix or data frame when using the \code{uniq.n} function.
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Extract Unique Elements, uniq() function
#'
#' # Example 1a: Extract unique elements in a vector
#' uniq(airquality, Ozone)
#'
#' # Example 1b: Extract unique elements in a vector, round elements
#' uniq(airquality, Wind, digits = 0)
#'
#' # Example 1b: Extract unique elements in a vector, do not sort
#' uniq(airquality, Ozone, sort = FALSE)
#'
#' # Example 1b: Extract unique elements in a vector, keep NA
#' uniq(airquality, Ozone, na.rm = FALSE)
#'
#' # Example 2a: Extract unique elements in a data frame
#' uniq(airquality)
#'
#' # Example 2a: Extract unique elements in list
#' uniq(airquality, table = FALSE)
#'
#' #----------------------------------------------------------------------------
#' # Count Number of Unique Elements, uniq.n() function
#'
#' # Example 3a: Count number of unique elements in a vector
#' uniq.n(airquality, Ozone)
#'
#' # Example 1b: Count number of unique elements for each variable in a data frame
#' uniq.n(airquality)
uniq <- function(data, ..., na.rm = TRUE, sort = TRUE, decreasing = FALSE, digits = NULL,
                 table = TRUE, write = NULL, append = TRUE, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a vector, factor, matrix, or data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Variable names
    var.names <- .var.names(data = data, ...)

    # Extract data and convert tibble into data frame or vector
    x <- data[, var.names] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Convert tibble into data frame or vector
    x <- data |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(logical = c("na.rm", "sort", "decreasing", "table"), args = "digits", envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dimension of the object = NULL ####

  if (isTRUE(is.null(dim(x)))) {

    # Round
    if (isTRUE(!is.null(digits) && is.numeric(x))) { x <- round(x, digits = digits) }

    # Extract unique elements
    result <- unique(x) |> (\(y) if (isTRUE(na.rm)) { as.vector(na.omit(y)) } else { y })() |> (\(z) if (isTRUE(sort)) { sort(z, decreasing = decreasing, na.last = TRUE) } else { z })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dimension of the object != NULL ####

  } else {

    # Round
    if (isTRUE(!is.null(digits) && any(sapply(x, is.numeric)))) { x[, sapply(x, is.numeric)] <- data.frame(round(x[, sapply(x, is.numeric)], digits = digits)) }

    # Extract unique elements as list
    result <- lapply(as.data.frame(x), function(y) misty::uniq(as.vector(y), na.rm = na.rm, sort = sort, check = FALSE, output = FALSE)$result)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "uniq",
                 data = x,
                 args = list(na.rm = na.rm, sort = sort, decreasing = decreasing, digits = digits, table = table, write = write, append = append, check = check, output = output),
                 result = result)

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
#_______________________________________________________________________________

#' @rdname uniq
uniq.n <- function(data, ..., na.rm = TRUE, digits = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a vector, factor, matrix, or data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Variable names
    var.names <- .var.names(..., data = data)

    # Extract data and convert tibble into data frame or vector
    x <- data[, var.names] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Convert tibble into data frame or vector
    x <- data |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(logical = "na.rm", args = "digits", envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dimension of the object = NULL ####

  if (isTRUE(is.null(dim(x)))) {

    # Round
    if (isTRUE(!is.null(digits) && is.numeric(x))) { x <- round(x, digits = digits) }

    # Number of unique elements
    object <- unique(x) |> (\(y) if (isTRUE(na.rm)) { length(na.omit(y)) } else { length(y) })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dimension of the object != NULL ####

  } else {

    # Round
    if (isTRUE(!is.null(digits) && any(sapply(x, is.numeric)))) { x[, sapply(x, is.numeric)] <- data.frame(round(x[, sapply(x, is.numeric)], digits = digits)) }

    # Number of unique elements
    object <- sapply(as.data.frame(x), misty::uniq.n, na.rm = na.rm)

  }

  return(object)

}

#_______________________________________________________________________________
