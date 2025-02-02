#' Replace User-Specified Values With Missing Values or Missing Values With
#' User-Specified Values
#'
#' The function \code{as.na} replaces user-specified values in the argument
#' \code{na} in a vector, factor, matrix, array, list, or data frame with
#' \code{NA}, while the function \code{na.as} replaces \code{NA} in a vector,
#' factor, matrix or data frame with user-specified values in the argument
#' \code{na}.
#'
#' @param ...     a vector, factor, matrix, array, data frame, or list.
#'                Alternatively, an expression indicating the variable names in
#'                \code{data} e.g., \code{as.na(x1, x2, data = dat)}. Note that
#'                the operators \code{.}, \code{+}, \code{-}, \code{~}, \code{:},
#'                \code{::}, and \code{!} can also be used to select variables,
#'                see 'Details' in the \code{\link{df.subset}} function.
#' @param data    a data frame when specifying one or more variables in the
#'                argument \code{...}. Note that the argument is \code{NULL}
#'                when specifying a vector, factor, matrix, array, data frame,
#'                or list for the argument \code{...}.
#' @param na      a vector indicating values or characters to replace with
#'                \code{NA}, or which \code{NA} is replaced.
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
#' argument \code{...} or a data frame specified in \code{data} with variables
#' specified in \code{...} replaced.
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Numeric vector
#' num <- c(1, 3, 2, 4, 5)
#'
#' # Example 1: Replace 2 with NA
#' as.na(num, na = 2)
#'
#' # Example 2: Replace 2, 3, and 4 with NA
#' as.na(num, na = c(2, 3, 4))
#'
#' # Example 3: Replace NA with 2
#' na.as(c(1, 3, NA, 4, 5), na = 2)
#'
#' #----------------------------------------------------------------------------
#' # Character vector
#' chr <- c("a", "b", "c", "d", "e")
#'
#' # Example 4: Replace "b" with NA
#' as.na(chr, na = "b")
#'
#' # Example 5: Replace "b", "c", and "d" with NA
#' as.na(chr, na = c("b", "c", "d"))
#'
#' # Example 6: Replace NA with "b"
#' na.as(c("a", NA, "c", "d", "e"), na = "b")
#'
#' #----------------------------------------------------------------------------
#' # Factor
#' fac <- factor(c("a", "a", "b", "b", "c", "c"))
#'
#' # Example 7: Replace "b" with NA
#' as.na(fac, na = "b")
#'
#' # Example 8: Replace "b" and "c" with NA
#' as.na(fac, na = c("b", "c"))
#'
#' # Example 9: Replace NA with "b"
#' na.as(factor(c("a", "a", NA, NA, "c", "c")), na = "b")
#'
#' #----------------------------------------------------------------------------
#' # Matrix
#' mat <- matrix(1:20, ncol = 4)
#'
#' # Example 10: Replace 8 with NA
#' as.na(mat, na = 8)
#'
#' # Example 11: Replace 8, 14, and 20 with NA
#' as.na(mat, na = c(8, 14, 20))
#'
#' # Example 12: Replace NA with 2
#' na.as(matrix(c(1, NA, 3, 4, 5, 6), ncol = 2), na = 2)
#'
#' #----------------------------------------------------------------------------
#' # Array
#'
#' # Example 13: Replace 1 and 10 with NA
#' as.na(array(1:20, dim = c(2, 3, 2)), na = c(1, 10))
#'
#' #----------------------------------------------------------------------------
#' # List
#'
#' # Example 14:  Replace 1 with NA
#' as.na(list(x1 = c(1, 2, 3, 1, 2, 3),
#'            x2 = c(2, 1, 3, 2, 1),
#'            x3 = c(3, 1, 2, 3)), na = 1)
#'
#' #----------------------------------------------------------------------------
#' # Data frame
#' df <- data.frame(x1 = c(1, 2, 3),
#'                  x2 = c(2, 1, 3),
#'                  x3 = c(3, 1, 2))
#'
#' # Example 15: Replace 1 with NA
#' as.na(df, na = 1)
#'
#' # Alternative specification using the 'data' argument
#' as.na(., data = df, na = 1)
#'
#' # Example 16: Replace 1 and 3 with NA
#' as.na(df, na = c(1, 3))
#'
#' # Example 17: Replace 1 with NA in 'x2'
#' as.na(df$x2, na = 1)
#'
#' # Alternative specification using the 'data' argument
#' as.na(x2, data = df, na = 1)
#'
#' # Example 18: Replace 1 with NA in 'x2' and 'x3'
#' as.na(x2, x3, data = df, na = 1)
#'
#' # Example 19: Replace 1 with NA in 'x1', 'x2', and 'x3'
#' as.na(x1:x3, data = df, na = 1)
#'
#' # Example 20: Replace NA with -99
#' na.as(data.frame(x1 = c(NA, 2, 3),
#'                  x2 = c(2, NA, 3),
#'                  x3 = c(3, NA, 2)), na = -99)
#'
#' # Example 2: Recode by replacing 30 with NA and then replacing NA with 3
#' na.as(data.frame(x1 = c(1, 2, 30),
#'                  x2 = c(2, 1, 30),
#'                  x3 = c(30, 1, 2)), na = 3, as.na = 30)
as.na <- function(..., data = NULL, na, replace = TRUE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'na' is missing
  if (isTRUE(missing(na))) { stop("Please specify a numeric vector or character vector for the argument 'na'.", call. = FALSE) }

  # Check if input 'na' is NULL
  if (isTRUE(is.null(na))) { stop("Input specified for the argument 'na' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(data), 1L, 3L))) { data <- as.data.frame(data) }

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a vector, factor, matrix, array, data frame, or list")

    # Extract variables
    x <- data[, var.names]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    # Extract data
    x <- eval(..., enclos = parent.frame())

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(x), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(x)) == 1L)) { x <- unlist(x) } else { x <- as.data.frame(x) } }

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
    if (isTRUE(is.list(x))) {

      na.x <- sapply(na, function(y) all(sapply(x, function(z) all(!y %in% z))))

    # Vector or matrix
    } else {

      na.x <- !na %in% as.vector(x)

    }

    if (isTRUE(any(na.x))) { warning(paste0(ifelse(sum(na.x) == 1L, "Value specified in the argument 'na' was not found in 'x': ", "Values specified in the argument 'na' were not found in 'x': "), paste(na[na.x], collapse = ", ")), call. = FALSE) }

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

  if (isTRUE(!is.null(data) && replace)) {

    data[, var.names] <- object

    object <- data

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________

#' @rdname as.na
na.as <- function(..., data = NULL, na, replace = TRUE, as.na = NULL, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input '...' is missing
  if (isTRUE(missing(...))) { stop("Please specify the argument '...'.", call. = FALSE) }

  # Check if input '...' is NULL
  if (isTRUE(is.null(substitute(...)))) { stop("Input specified for the argument '...' is NULL.", call. = FALSE) }

  # Check if input 'na' is missing
  if (isTRUE(missing(na))) { stop("Please specify a numeric vector or character vector for the argument 'na'.", call. = FALSE) }

  # Check if input 'na' is NULL
  if (isTRUE(is.null(na))) { stop("Input specified for the argument 'na' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument 'data' ####

  if (isTRUE(!is.null(data))) {

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(data), 1L, 3L))) { data <- as.data.frame(data) }

    # Variable names
    var.names <- .var.names(..., data = data, check.chr = "a vector, factor, matrix, array, data frame, or list")

    # Extract variables
    x <- data[, var.names]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument 'data' ####

  } else {

    x <- eval(..., enclos = parent.frame())

    # Convert tibble into data frame
    if (isTRUE("tbl" %in% substr(class(x), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(x)) == 1L)) { x <- unlist(x) } else { x <- as.data.frame(x) } }

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

    # Vector, factor, matrix or data frame for the argument 'x'?
    if (isTRUE(!is.atomic(x) && !is.factor(x) && !is.matrix(x) && !is.data.frame(x))) { stop("Please specifiy a vector, factor, matrix or data frame for the argument 'x'.", call. = FALSE) }

    # Factor or Vector
    if (isTRUE(is.null(dim(x)))) {

      if (isTRUE(all(!is.na(x)))) { warning("There are no missing values in the vector or factor specified in 'x'.", call. = FALSE) }

    # Matrix or data frame
    } else {

      if (isTRUE(all(apply(x, 2, function(y)  all(!is.na(y)))))) { warning("There are no missing values in the matrix or data frame specified in 'x'.", call. = FALSE) }

    }

    # Check input 'na'
    if (isTRUE(length(na) != 1L)) { stop("Please specifiy a single value or character string for the argument 'na'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  # Factor or Vector
  if (isTRUE(is.null(dim(x)))) {

    # Factor
    if (isTRUE(is.factor(x))) {

      # Factor levels
      f.levels <- sort(unique(as.numeric(x)))

      f.value <- length(f.levels) + 1L
      f.levels <- c(f.levels, f.value)

      # Factor labels
      f.labels <- c(levels(x), na)

      object <- factor(ifelse(is.na(x), f.value, x), levels = f.levels, labels = f.labels)

    # Vector
    } else {

      object <- ifelse(is.na(x), na, x)

    }

  # Matrix or data frame
  } else {

    # Matrix
    if (isTRUE(is.matrix(x))) {

      object <- apply(x, 2L, na.as, na = na, check = FALSE)

    }

    # Data frame
    if (isTRUE(is.data.frame(x))) {

      object <- data.frame(lapply(x, na.as, na = na, check = FALSE), check.names = FALSE, fix.empty.names = FALSE)

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Replace ####

  if (isTRUE(!is.null(data) && replace)) {

    data[, var.names] <- object

    object <- data

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
