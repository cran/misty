#' Rename Columns in a Matrix or Variables in a Data Frame
#'
#' This function renames columns in a matrix or variables in a data frame by specifying a character string or
#' character vector indicating the columns or variables to be renamed and a character string or character
#' vector indicating the corresponding replacement values.
#'
#' @param x           a matrix or data frame.
#' @param from        a character string or character vector indicating the column(s) or variable(s) to be renamed.
#' @param to          a character string or character vector indicating the corresponding replacement values for
#'                    the column(s) or variable(s) specified in the argument \code{name}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{df.duplicated}}, \code{\link{df.unique}}, \code{\link{df.merge}}, \code{\link{df.rbind}},
#' \code{\link{df.sort}}
#'
#' @return
#' Returns a matrix or data frame with renamed columns or variables.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(a = c(3, 1, 6),
#'                   b = c(4, 2, 5),
#'                   c = c(7, 3, 1))
#'
#' # Rename variable b in the data frame 'dat' to y
#' df.rename(dat, from = "b", to = "y")
#'
#' # Rename variabley a, b, and c in the data frame 'dat' to x, y, and z
#' df.rename(dat, from = c("a", "b", "c"), to = c("x", "y", "z"))
df.rename <- function(x, from, to, check = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check input 'from'
  if (missing(from)) {

    stop("Please specify a character string or character vector for the argument 'from'.",
         call. = FALSE)

  }

  #......
  # Check input 'to'
  if (missing(to)) {

    stop("Please specify a character string or character vector for the argument 'to'.",
         call. = FALSE)

  }

  #......
  # Check input 'check'
  if (!is.logical(check)) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #.........................
    # Character string or vector for the argument 'from'?
    if (!is.character(from)) {

      stop("Please specify a character string or character vector for the argument 'from'.",
           call. = FALSE)

    }

    #.........................
    # Character string or vector for the argument 'to'?
    if (!is.character(to)) {

      stop("Please specify a character string or character vector for the argument 'to'.",
           call. = FALSE)

    }

    #.........................
    # Vector in argument 'from' matching with the vector in argument 'to'?

    if (length(from) != length(to)) {

      stop("Length of the vector specified in 'from' does not match with the vector specified in 'to'.",
           call. = FALSE)

    }

    #.........................
    # Variables specified in the argument 'from' in 'x'?

    if (any(!from %in% colnames(x))) {

      var.from <- from[which(!from %in% colnames(x))]

      stop("Column name(s) specified in 'from' was not found in 'x': ", paste(var.from, collapse = ", "),
           call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # Matrix

  if (is.matrix(x)) {

    colnames(x)[match(from, colnames(x))] <- to

    # Duplicated columns from
    if (anyDuplicated(colnames(x))) {

      warning(paste0("Duplicated column names in the matrix after renaming columns: ",
                     paste(unique(colnames(x)[duplicated(colnames(x))]), collapse = ", ")), call. = FALSE)

    }

  #-----------------------------------------
  # Data frame

  } else {

    names(x)[match(from, names(x))] <- to

    # Duplicated variable names
    if (anyDuplicated(names(x))) {

      warning(paste0("Duplicated variable names in the data frame after renaming variables: ",
                     paste(unique(names(x)[duplicated(names(x))]), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Return object

  return(x)

}
