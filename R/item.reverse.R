#' Reverse Code Scale Item
#'
#' This function reverse codes inverted items, i.e., items that are negatively
#' worded.
#'
#' If arguments \code{min} and/or \code{max} are not specified, empirical minimum
#' and/or maximum is computed from the data Note, however, that reverse coding
#' might fail if the lowest or highest possible scale value is not represented in
#' the data That is, it is always preferable to specify the arguments \code{min}
#' and \code{max}.
#'
#' @param x     a numeric vector for reverse coding an item, matrix or data frame
#'              for reverse coding more than one item.
#' @param min   an integer indicating the minimum of the item (i.e., lowest possible
#'              scale value).
#' @param max   an integer indicating the maximum of the item (i.e., highest possible
#'              scale value).
#' @param keep  a numeric vector indicating values not to be reverse coded.
#' @param names a character string or character vector indicating the names
#'              of the reverse coded item when specifying more than one variable.
#'              By default, variables are named with the ending \code{".r"}
#'              resulting in e.g. \code{"x1.r"} and \code{"x2.r"}. Variable names
#'              can also be specified using a character vector matching the number
#'              of variables specified in \code{x} (e.g.,
#'              \code{names = c("reverse.x1", "reverse.x2")}).
#' @param as.na a numeric vector indicating user-defined missing values, i.e. these
#'              values are converted to \code{NA} before conducting the analysis.
#' @param table logical: if \code{TRUE}, a cross table item x reverse coded item
#'              is printed on the console if only one variable is specified in
#'              \code{x}.
#' @param check logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{item.alpha}}, \code{\link{item.omega}}, \code{\link{rec}},
#' \code{\link{item.scores}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology
#' - Using R and SPSS}. New York: John Wiley & Sons.
#'
#' @return
#' Returns a numeric vector or data frame with the same length or same number of
#' rows as \code{x} containing the reverse coded scale item(s).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(item1 = c(1, 5, 3, 1, 4, 4, 1, 5),
#'                   item2 = c(1, 1.3, 1.7, 2, 2.7, 3.3, 4.7, 5),
#'                   item3 = c(4, 2, 4, 5, 1, 3, 5, -99))
#'
#' # Reverse code item1
#' dat$item1r <- item.reverse(dat$item1, min = 1, max = 5)
#'
#' # Reverse code item3 while keeping the value -99
#' dat$item3r <- item.reverse(dat$item3, min = 1, max = 5, keep = -99)
#'
#' # Reverse code item3 while keeping the value -99 and check recoding
#' dat$item3r <- item.reverse(dat$item3, min = 1, max = 5, keep = -99, table = TRUE)
#'
#' # Reverse code item1, item2, and item 3 and attach to 'dat'
#' dat <- cbind(dat,
#'              item.reverse(dat[, c("item1", "item2", "item3")],
#'                           min = 1, max = 5, keep = -99))
item.reverse <- function(x, min = NULL, max = NULL, keep = NULL, names = ".r",
                         as.na = NULL, table = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Convert 'x' into a vector when only one variable specified in 'x'
  if (isTRUE(ncol(data.frame(x)) == 1L)) { x <- unlist(x, use.names = FALSE) }

  # Argument 'min'
  if (isTRUE(is.null(min))) { min <- min(x, na.rm = TRUE) }

  # Argument 'max'
  if (isTRUE(is.null(max))) { max <- max(x, na.rm = TRUE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'x'
    if (isTRUE(any(vapply(data.frame(x), mode, FUN.VALUE = character(1L)) != "numeric"))) {

      if (isTRUE(is.null(dim(x)))) {

        stop("Please specify a numeric vector for the argument 'x'.", call. = FALSE)

      } else {

        stop("Please specify a matrix or data frame with numeric vectors for the argument 'x'.", call. = FALSE)

      }

    }

    # Check input 'min'
    if (isTRUE(length(min) !=  1L || !is.numeric(min) || min %% 1L != 0L)) { stop("Please specify a single integer value for the argument 'min'.", call. = FALSE) }

    # Check input 'max'
    if (isTRUE(length(max) !=  1L || !is.numeric(max) || max %% 1L != 0L)) { stop("Please specify a single integer value for the argument 'max'.", call. = FALSE) }

    # Check input 'keep'
    keep.na <- !keep %in% x
    if (isTRUE(any(keep.na))) {

      warning(paste0("Values specified in the argument 'keep' were not found in 'x': ", paste(keep[keep.na], collapse = ", ")), call. = FALSE)

    }

    # Check input 'names'
    if (isTRUE(!is.null(dim(x)))) {

      if (isTRUE(!is.character(names))) { stop("Please specify a character string or vector for the argument 'names'.", call. = FALSE) }

      if (isTRUE(length(names) > 1L && length(names) != ncol(x))) {  stop("The length of the vector specified in 'names' does not match with the number of variable in 'x'.", call. = FALSE) }

    }

    # Check input 'table'
    if (isTRUE(!is.logical(table))) { stop("Please specify TRUE or FALSE for the argument 'table'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Data and Arguments ---------------------------------------------------------

  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    if (isTRUE(all(is.na(x)))) { stop("After converting user-missing values into NA, variable 'x' is completely missing.", call. = FALSE) }

  }

  # Lowest number
  low <- min

  # Highest number
  high <- max

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Single variable ####
  if (isTRUE(is.null(dim(x)))) {

    # Reverse coded vector
    object <- x

    # Keep values, i.e., not to be reverse coded
    if (isTRUE(!is.null(keep))) {

      object[object %in% keep] <- NA

    }

    # Reverse code vector
    object <- sum(low, high) - object


    # Values not to be reverse coded
    if (isTRUE(!is.null(keep))) {

      object[which(x %in% keep)] <- x[which(x %in% keep)]

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Multiple variables ####
  } else {

    object <- data.frame(vapply(x, misty::item.reverse, min = min, max = max,
                                keep = keep, as.na = as.na, table = FALSE,
                                check = FALSE, FUN.VALUE = double(nrow(x))))

    #...................
    ### Variable names ####

    if (isTRUE(length(names) == 1L)) {

      colnames(object) <- paste0(colnames(object), names)

    } else {

      colnames(object) <- names

    }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print cross table ####

  if (isTRUE(is.null(dim(x)) && table)) {

    print(table(x, object, dnn = c("item", "reverse coded")))

    return(invisible(object))

  } else {

    return(object)

  }

}
