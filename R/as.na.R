#' Replace User-Specified Values With Missing Values or Missing Values With
#' User-Specified Values
#'
#' The function \code{as.na} replaces user-specified values in the argument
#' \code{na} in a vector, factor, matrix, array, list, or data frame with
#' \code{NA}, while the function \code{na.as} replaces \code{NA} in a vector,
#' factor, matrix or data frame with a user-specified value or character string
#' in the argument \code{na}.
#'
#' @param data    a vector, factor, matrix, array, data frame, or list.
#' @param ...     an expression indicating the variable names in \code{data}, e.g.,
#'                \code{as.na(dat, x1, x2)} for selecting the variables \code{x1}
#'                and \code{x2} from the data frame \code{dat}. Note that the
#'                operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                \code{::}, and \code{!} can also be used to select variables,
#'                see 'Details' in the \code{\link{df.subset}} function.
#' @param na      a vector indicating values or characters to replace with
#'                \code{NA}, or which \code{NA} is replaced. Note that a numeric
#'                value or character string needs to be specified for the argument
#'                \code{na} when using \code{na.as}.
#' @param replace logical: if \code{TRUE} (default), variable(s) specified in
#'                \code{...} are replaced in the argument \code{data}.
#' @param as.na   a numeric vector or character vector indicating user-defined
#'                missing values, i.e. these values are converted to \code{NA}
#'                before conducting the analysis.
#' @param check   logical: if \code{TRUE} (default), argument specification is
#'                checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @name as.na
#'
#' @seealso
#' \code{\link{na.auxiliary}}, \code{\link{na.coverage}}, \code{\link{na.descript}},
#' \code{\link{na.indicator}}, \code{\link{na.pattern}}, \code{\link{na.prop}},
#' \code{\link{na.test}}
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}.
#' Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns a vector, factor, matrix, array, data frame, or list specified in the
#' argument \code{data}.
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Numeric vector
#' num <- c(1, 3, 2, 4, 5)
#'
#' # Example 1a: Replace 2 with NA
#' as.na(num, na = 2)
#'
#' # Example 1b: Replace 2, 3, and 4 with NA
#' as.na(num, na = c(2, 3, 4))
#'
#' # Example 1c: Replace NA with 2
#' na.as(c(1, 3, NA, 4, 5), na = 2)
#'
#' #----------------------------------------------------------------------------
#' # Character vector
#' chr <- c("a", "b", "c", "d", "e")
#'
#' # Example 2a: Replace "b" with NA
#' as.na(chr, na = "b")
#'
#' # Example 2b: Replace "b", "c", and "d" with NA
#' as.na(chr, na = c("b", "c", "d"))
#'
#' # Example 2c: Replace NA with "b"
#' na.as(c("a", NA, "c", "d", "e"), na = "b")
#'
#' #----------------------------------------------------------------------------
#' # Factor
#' fac <- factor(c("a", "a", "b", "b", "c", "c"))
#'
#' # Example 3a: Replace "b" with NA
#' as.na(fac, na = "b")
#'
#' # Example 3b: Replace "b" and "c" with NA
#' as.na(fac, na = c("b", "c"))
#'
#' # Example 3c: Replace NA with "b"
#' na.as(factor(c("a", "a", NA, NA, "c", "c")), na = "b")
#'
#' #----------------------------------------------------------------------------
#' # Matrix
#' mat <- matrix(1:20, ncol = 4)
#'
#' # Example 4a: Replace 8 with NA
#' as.na(mat, na = 8)
#'
#' # Example 4b: Replace 8, 14, and 20 with NA
#' as.na(mat, na = c(8, 14, 20))
#'
#' # Example 4c: Replace NA with 2
#' na.as(matrix(c(1, NA, 3, 4, 5, 6), ncol = 2), na = 2)
#'
#' #----------------------------------------------------------------------------
#' # Array
#'
#' # Example 5: Replace 1 and 10 with NA
#' as.na(array(1:20, dim = c(2, 3, 2)), na = c(1, 10))
#'
#' #----------------------------------------------------------------------------
#' # List
#'
#' # Example 6:  Replace 1 with NA
#' as.na(list(x1 = c(1, 2, 3, 1, 2, 3), x2 = c(2, 1, 3, 2, 1)), na = 1)
#'
#' #----------------------------------------------------------------------------
#' # Data frame
#' df <- data.frame(x1 = c(1, 2, 3), x2 = c(2, 1, 3), x3 = c(3, 1, 2))
#'
#' # Example 7a: Replace 1 with NA
#' as.na(df, na = 1)
#'
#' # Example 7b: Replace 1 with NA for the variable x2
#' as.na(df, x2, na = 1)
#'
#' # Alternative specification
#' as.na(df$x2, na = 1)
#'
#' # Example 7c: Replace 1 and 3 with NA
#' as.na(df, na = c(1, 3))
#'
#' # Example 7d: Replace 1 with NA in 'x2' and 'x3'
#' as.na(df, x2, x3, na = 1)
#'
#' # Example 7e: Replace NA with -99
#' na.as(data.frame(x1 = c(NA, 2, 3), x2 = c(2, NA, 3)), na = -99)
#'
#' # Example 7f: Recode by replacing 30 with NA and then replacing NA with 3
#' na.as(data.frame(x1 = c(1, 2, 30), x2 = c(2, 1, 30)), na = 3, as.na = 30)
as.na <- function(data, ..., na, replace = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a vector, factor, matrix, array, data frame, or list for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  # Check if input 'na' is missing
  if (isTRUE(missing(na))) { stop("Please specify a numeric vector or character vector for the argument 'na'.", call. = FALSE) }

  # Check if input 'na' is NULL
  if (isTRUE(is.null(na))) { stop("Input specified for the argument 'na' is NULL.", call. = FALSE) }

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

  # Check input 'replace'
  .check.input(logical = "replace", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Values in 'na' ####

    # Data frame or list
    if (isTRUE(is.list(data))) {

      na.data <- sapply(na, function(y) all(sapply(data, function(z) all(!y %in% z))))

    # Vector or matrix
    } else {

      na.data <- !na %in% as.vector(data)

    }

    if (isTRUE(any(na.data))) { warning(paste0(ifelse(sum(na.data) == 1L, "Value specified in the argument 'na' was not found in 'data': ", "Values specified in the argument 'na' were not found in 'data': "), paste(na[na.data], collapse = ", ")), call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dimension of the object = NULL ####
  if (isTRUE(is.null(dim(x)))) {

    #...................
    ### Atomic ####

    if (isTRUE(is.atomic(x))) {

      # Factor
      if (isTRUE(is.factor(x))) {

        f.levels <- sort(unique(as.numeric(x)))
        f.labels <- levels(x)

        if (isTRUE(any(na %in% f.labels))) {

          f.levels <- f.levels[-which(f.labels %in% na)]
          f.labels <- f.labels[-which(f.labels %in% na)]

        }

        object <- setNames(factor(ifelse(x %in% na, NA, x), levels = f.levels, labels = f.labels), nm = names(x))

      # Vector
      } else { object <- setNames(ifelse(x %in% na, NA, x), nm = names(x)) }

    #...................
    ### List ####
    } else if (isTRUE(is.list(x))) { object <- lapply(x, misty::as.na, na = na, check = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dimension of the object != NULL ####

  #...................
  ### Matrix ####
  } else if (isTRUE(is.matrix(x))) { object <- apply(x, 2L, misty::as.na, na = na, check = FALSE)

  #...................
  ### Array ####
  } else if (isTRUE(is.array(x))) { object <- array(sapply(x, misty::as.na, na = na, check = FALSE), dim = dim(x))

  #...................
  ### Data frame ####
  } else if (isTRUE(is.data.frame(x))) { object <- data.frame(lapply(x, misty::as.na, na = na, check = FALSE), stringsAsFactors = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Replace ####

  if (isTRUE(!missing(...) && replace)) { object <- data[, var.names] <- object }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________

#' @rdname as.na
na.as <- function(data, ..., na, replace = TRUE, as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a vector, factor, matrix, array, data frame, or list for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  # Check if input 'na' is missing
  if (isTRUE(missing(na))) { stop("Please specify a numeric vector or character vector for the argument 'na'.", call. = FALSE) }

  # Check if input 'na' is NULL
  if (isTRUE(is.null(na))) { stop("Input specified for the argument 'na' is NULL.", call. = FALSE) }

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

    # Extract data and convert tibble into data frame or vector
    x <- data |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  }

  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) { x <- misty::as.na(x, na = as.na, check = check) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'replace'
  .check.input(logical = "replace", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Vector, factor, matrix or data frame for the argument 'data'?
    if (isTRUE(!is.atomic(x) && !is.factor(x) && !is.matrix(x) && !is.data.frame(x))) { stop("Please specifiy a vector, factor, matrix or data frame for the argument 'data'.", call. = FALSE) }

    # Factor or Vector
    if (isTRUE(is.null(dim(x)))) {

      if (isTRUE(all(!is.na(x)))) { warning("There are no missing values in the vector or factor specified in 'data'.", call. = FALSE) }

    # Matrix or data frame
    } else {

      if (isTRUE(all(apply(x, 2L, function(y)  all(!is.na(y)))))) { warning("There are no missing values in the matrix or data frame specified in 'data'.", call. = FALSE) }

    }

    # Check input 'na'
    if (isTRUE(length(na) != 1L)) { stop("Please specifiy a single value or character string for the argument 'na'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dimension of the object = NULL ####
  if (isTRUE(is.null(dim(data)))) {

    #...................
    ### Atomic ####

    if (isTRUE(is.atomic(data))) {

      # Factor
      if (isTRUE(is.factor(data))) {

        # Factor levels
        f.levels <- sort(unique(as.numeric(data)))

        f.value <- length(f.levels) + 1L
        f.levels <- c(f.levels, f.value)

        # Factor labels
        f.labels <- c(levels(data), na)

        object <- factor(ifelse(is.na(data), f.value, data), levels = f.levels, labels = f.labels)

      # Vector
      } else { object <- ifelse(is.na(data), na, data) }

    #...................
    ### List ####
    } else if (isTRUE(is.list(data))) { object <- lapply(data, misty::na.as, na = na, check = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dimension of the object != NULL ####

  #...................
  ### Matrix ####
  } else if (isTRUE(is.matrix(data))) { object <- apply(data, 2L, misty::na.as, na = na, check = FALSE)

  #...................
  ### Array ####
  } else if (isTRUE(is.array(data))) { object <- array(sapply(data, misty::na.as, na = na, check = FALSE), dim = dim(data))

  #...................
  ### Data frame ####
  } else if (isTRUE(is.data.frame(data))) { object <- data.frame(lapply(data, na.as, na = na, check = FALSE), check.names = FALSE, fix.empty.names = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Replace ####

  if (isTRUE(!missing(...) && replace)) { object <- data[, var.names] <- object }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
