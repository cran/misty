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
#' @param data   a numeric vector for reverse coding an item or data frame for
#'               reverse coding more than one item.
#' @param ...    an expression indicating the variable names in \code{data} e.g.,
#'               \code{item.reverse(x1, x2, x3, data = dat)}. Note that the operators
#'               \code{.}, \code{+}, \code{-}, \code{~}, \code{:}, \code{::},
#'               and \code{!} can also be used to select variables, see 'Details'
#'               in the \code{\link{df.subset}} function.
#' @param min    an integer indicating the minimum of the item (i.e., lowest
#'               possible scale value).
#' @param max    an integer indicating the maximum of the item (i.e., highest
#'               possible scale value).
#' @param keep   a numeric vector indicating values not to be reverse coded.
#' @param append logical: if \code{TRUE} (default), centered variable(s) are
#'               appended to the data frame specified in the argument \code{data}.
#' @param name   a character string or character vector indicating the names
#'               of the reverse coded item. By default, variables are named with
#'               the ending \code{".r"} resulting in e.g. \code{"x1.r"} and
#'               \code{"x2.r"}. Variable names can also be specified using a
#'               character vector matching the number of variables (e.g.,
#'               \code{name = c("reverse.x1", "reverse.x2")}).
#' @param as.na  a numeric vector indicating user-defined missing values, i.e.
#'               these values are converted to \code{NA} before conducting the
#'                analysis.
#' @param table  logical: if \code{TRUE}, a cross table item x reverse coded item
#'               is printed on the console if only one variable is specified.
#' @param check  logical: if \code{TRUE} (default), argument specification is checked.
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
#' - Using R and SPSS}. John Wiley & Sons.
#'
#' @return
#' Returns a numeric vector or data frame with the same length or same number of
#' rows as \code{data} containing the reverse coded scale item(s).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(item1 = c(1, 5, 3, 1, 4, 4, 1, 5),
#'                   item2 = c(1, 1.3, 1.7, 2, 2.7, 3.3, 4.7, 5),
#'                   item3 = c(4, 2, 4, 5, 1, 3, 5, -99))
#'
#' # Example 1: Reverse code 'item1' and append to 'dat'
#' item.reverse(dat, item1, min = 1, max = 5)
#'
#' # Alternative specification without using the '...' argument
#' item.reverse(dat$item1, min = 1, max = 5)
#'
#' # Example 2: Reverse code 'item3' while keeping the value -99
#' item.reverse(dat, item3, min = 1, max = 5, keep = -99)
#'
#' # Example 3: Reverse code 'item3' while keeping the value -99 and check recoding
#' item.reverse(dat, item3, min = 1, max = 5, keep = -99, table = TRUE)
#'
#' # Example 4: Reverse code 'item1', 'item2', and 'item3' and attach to 'dat'
#' item.reverse(item1:item3, data = dat, min = 1, max = 5, keep = -99)
#'
#' # Alternative specification without using the '...' argument
#' dat <- cbind(dat,
#'              item.reverse(dat[, c("item1", "item2", "item3")],
#'                           min = 1, max = 5, keep = -99))
item.reverse <- function(data, ..., min = NULL, max = NULL, keep = NULL,
                         append = TRUE, name = ".r", as.na = NULL, table = FALSE,
                         check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a numeric vector or data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!missing(...))) {

    # Variable names
    var.names <- .var.names(..., data = data)

    # Extract data and convert tibble into data frame or vector
    x <- data[, var.names] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Convert 'data' as tibble into data frame
    x <- data |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

    # Data and cluster
    var.group <- .var.group(data = x)

    # Data
    if (isTRUE(!is.null(var.group$data))) { x <- var.group$data }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Argument ####

  # Argument 'min'
  if (isTRUE(is.null(min))) { min <- min(x, na.rm = TRUE) }

  # Argument 'max'
  if (isTRUE(is.null(max))) { max <- max(x, na.rm = TRUE) }

  # Lowest number
  low <- min

  # Highest number
  high <- max

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("append", "table"), numeric = list(min = 1L, max = 1L), envir = environment(), input.check = check)

  if (isTRUE(check)) {

    # Check input 'data'
    if (isTRUE(any(vapply(data.frame(x), mode, FUN.VALUE = character(1L)) != "numeric"))) {

      if (isTRUE(is.null(dim(x)))) {

        stop("Please specify a numeric vector for the argument 'data'.", call. = FALSE)

      } else {

        stop("Please specify a data frame with numeric vectors for the argument 'data'.", call. = FALSE)

      }

    }

    # Check input 'min'
    if (isTRUE(length(min) !=  1L || !is.numeric(min) || min %% 1L != 0L)) { stop("Please specify a single integer value for the argument 'min'.", call. = FALSE) }

    # Check input 'max'
    if (isTRUE(length(max) !=  1L || !is.numeric(max) || max %% 1L != 0L)) { stop("Please specify a single integer value for the argument 'max'.", call. = FALSE) }

    # Check input 'keep'
    keep.na <- !keep %in% unlist(x)
    if (isTRUE(any(keep.na))) {

      warning(paste0("Values specified in the argument 'keep' were not found in 'data': ", paste(keep[keep.na], collapse = ", ")), call. = FALSE)

    }

    # Check input 'name'
    if (isTRUE(!is.null(dim(x)))) {

      if (isTRUE(!is.character(name))) { stop("Please specify a character string or vector for the argument 'name'.", call. = FALSE) }

      if (isTRUE(length(name) > 1L && length(name) != ncol(x))) {  stop("The length of the vector specified in 'name' does not match with the number of variable in 'data'.", call. = FALSE) }

    }

  }

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

    object <- data.frame(vapply(x, misty::item.reverse, min = min, max = max, keep = keep, as.na = as.na, table = FALSE,
                                check = FALSE, FUN.VALUE = double(nrow(x))))

    #...................
    ### Variable names ####

    if (isTRUE(length(name) == 1L)) {

      colnames(object) <- paste0(colnames(object), name)

    } else {

      colnames(object) <- name

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print cross table ####

  if (isTRUE(is.null(dim(x)) && table)) { print(table(x, object, dnn = c("item", "reverse coded"))) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!missing(...) && append)) {

    if (isTRUE(is.null(dim(x)))) {

      #...................
      ### Variable names ####

      if (isTRUE(name == ".r")) {

        object <- setNames(as.data.frame(object), nm = paste0(var.names, ".r"))

      } else {

        object <- setNames(as.data.frame(object), nm = name)

      }

    }

    object <- data.frame(data, object)

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  if (isTRUE(is.null(dim(x)) && table)) { return(invisible(object)) } else { return(object) }

}

#_______________________________________________________________________________
