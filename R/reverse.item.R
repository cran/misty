#' Reverse Code Scale Item
#'
#' This function reverse codes an inverted item, i.e., item that is negatively worded.
#'
#' If arguments \code{min} and/or \code{max} are not specified, empirical minimum and/or maximum is computed
#' from the vector. Note, however, that reverse coding might fail if the lowest or highest possible scale value
#' is not represented in the vector. That is, it is always preferable to specify the arguments \code{min} and
#' \code{max}.
#'
#' @param x           a numeric vector with integer values.
#' @param min         an integer indicating the minimum of the item (i.e., lowest possible scale value).
#' @param max         an integer indicating the maximum of the item (i.e., highest possible scale value).
#' @param keep        a numeric vector indicating values not to be reverse coded.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param table       logical: if \code{TRUE}, a cross table item x reverse coded item is printed
#'                    on the console.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{alpha.coef}}, \code{\link{omega.coef}}, \code{\link{rec}}, \code{\link{scores}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @return
#' Returns a numeric vector with the same length as \code{x} containing the reverse coded scale item.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(item1 = c(5, 2, 3, 4, 1, 2, 4, 2),
#'                   item2 = c(1, 5, 3, 1, 4, 4, 1, 5),
#'                   item3 = c(4, 2, 4, 5, 1, 3, 5, -99))
#'
#' # Reverse code item2
#' reverse.item(dat$item2, min = 1, max = 5)
#'
#' # Reverse code item3 while keeping the value -99
#' reverse.item(dat$item3, min = 1, max = 5, keep = -99)
#'
#' # Reverse code item3 while keeping the value -99 and check recoding
#' dat$item3r <- reverse.item(dat$item3, min = 1, max = 5, keep = -99, table = TRUE)
reverse.item <- function(x, min = NULL, max = NULL, keep = NULL, as.na = NULL,
                         table = FALSE, check = TRUE) {

  ####################################################################################
  # Input check

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a numeric vector with integer values for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (is.null(x)) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Argument 'min'
  if (is.null(min)) {

    min <- min(x, na.rm = TRUE)

  }

  #......
  # Argument 'max'
  if (is.null(max)) {

    max <- max(x, na.rm = TRUE)

  }

  #......
  # Check input 'check'
  if (!is.logical(check)) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x': Numeric vector
    if (!is.numeric(x)) {

      stop("Please specify a numeric vector with integer values for the argument 'x'.")

    }

    #......
    # Check input 'x': Integer values
    if (any(na.omit(x) %% 1 != 0)) {

      stop("Please specify a numeric vector with integer values for the argument 'x'.")

    }

    #......
    # Check input 'min'
    if (length(min) !=  1 || !is.numeric(min) || min %% 1 != 0) {

      stop("Please specify a single integer value for the argument 'min'.")

    }

    #......
    # Check input 'max'
    if (length(max) !=  1 || !is.numeric(max) || max %% 1 != 0) {

      stop("Please specify a single integer value for the argument 'max'.")

    }

    #......
    # Check input 'keep'
    keep.na <- !keep %in% x
    if (any(keep.na)) {

      warning(paste0("Values specified in the argument 'keep' were not found in 'x': ",
                     paste(keep[keep.na], collapse = ", ")), call. = FALSE)

    }

    #......
    # Check input 'table'
    if (!is.logical(table)) {

      stop("Please specify TRUE or FALSE for the argument 'table'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data

  # Reverse coded vector
  object <- x

  # Convert user-missing values into NA
  if (!is.null(as.na)) {

    object <- misty::as.na(object, as.na = as.na, check = check)

  }

  # Keep values, i.e., not to be reverse coded
  if (!is.null(keep)) {

    object[object %in% keep] <- NA

  }

  # Lowest number
  low <- min

  # Highest number
  high <- max

  ####################################################################################
  # Main Function

  # Reverse code vector
  object <- sum(low, high) - object


  # Values not to be reverse coded
  if (!is.null(keep)) {

    object[which(x %in% keep)] <- x[which(x %in% keep)]

  }

  ####################################################################################
  # Return object

  #----------------------------------------
  # Print cross table
  if (isTRUE(table)) {

    print(table(x, object, dnn = c("item", "reverse coded")))

    return(invisible(object))

  } else {

    return(object)

  }

}
