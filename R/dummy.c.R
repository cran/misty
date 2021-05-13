#' Dummy Coding
#'
#' This function creates \eqn{k - 1} dummy coded 0/1 variables for a vector with k distinct values.
#'
#' @param x           a numeric vector with integer values, character vector or factor.
#' @param ref         a numeric value or character string indicating the reference group.
#'                    By default, the last category is selected as reference group.
#' @param names       a character string or character vector indicating the names of the dummy variables.
#'                    By default, variables are named \code{"d"} with the category compared to the reference
#'                    category (e.g., \code{"d1"} and \code{"d2"}). Variable names can be specified using
#'                    a character string (e.g., \code{names = "dummy_"} leads to \code{dummy_1} and
#'                    \code{dummy_2}) or a character vector matching the number of dummy coded variables
#'                    (e.g. \code{names = c("x.3_1", "x.3_2")}) which is the number of unique categories minus
#'                    one.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @return
#' Returns a matrix with k - 1 dummy coded 0/1 variables.
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                   y = c("a", "a", "a", "b", "b", "b", "c", "c", "c"),
#'                   z = factor(c("B", "B", "B", "A", "A", "A", "C", "C", "C")),
#'                   stringsAsFactors = FALSE)
#'
#' # Dummy coding of a numeric variable, reference = 3
#' dummy.c(dat$x)
#'
#' # Dummy coding of a numeric variable, reference = 1
#' dummy.c(dat$x, ref = 1)
#'
#' # Dummy coding of a numeric variable, reference = 3
#' # assign user-specified variable names
#' dummy.c(dat$x, names = c("x.3_1", "x.3_2"))
#'
#' # Dummy coding of a numeric variable, reference = 3
#' # assign user-specified variable names and attach to the data frame
#' dat <- data.frame(dat, dummy.c(dat$x, names = c("x.3_1", "x.3_2")), stringsAsFactors = FALSE)
#'
#' # Dummy coding of a character variable, reference = "c"
#' dummy.c(dat$y)
#'
#' # Dummy coding of a character variable, reference = "a"
#' dummy.c(dat$y, ref = "a")
#'
#' # Dummy coding of a numeric variable, reference = "c"
#' # assign user-specified variable names
#' dummy.c(dat$y, names = c("y.c_a", "y.c_b"))
#'
#' # Dummy coding of a character variable, reference = "c"
#' # assign user-specified variable names and attach to the data frame
#' dat <- data.frame(dat, dummy.c(dat$y, names = c("y.c_a", "y.c_b")), stringsAsFactors = FALSE)
#'
#' # Dummy coding of a factor, reference = "C"
#' dummy.c(dat$z)
#'
#' # Dummy coding of a factor, reference = "A"
#' dummy.c(dat$z, ref = "A")
#'
#' # Dummy coding of a numeric variable, reference = "C"
#' # assign user-specified variable names
#' dummy.c(dat$z, names = c("z.C_A", "z.C_B"))
#'
#' # Dummy coding of a factor, reference = "C"
#' # assign user-specified variable names and attach to the data frame
#' dat <- data.frame(dat, dummy.c(dat$z, names = c("z.C_A", "z.C_B")), stringsAsFactors = FALSE)
dummy.c <- function(x, ref = NULL, names = "d", as.na = NULL, check = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a numeric vector with integer values, character vector or factor for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Vector or factor for the argument 'x'?
  if (isTRUE(!is.atomic(x) && !is.factor(x))) {

    stop("Please specify a numeric vector with integer values, character vector or factor for the argument 'x'.",
         call. = FALSE)

  }

  #-----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable is missing values only?
    if (isTRUE(all(is.na(x)))) {

      stop("After converting user-missing values into NA, 'x' is completely missing.", call. = FALSE)

    }

    # One unique value
    if (isTRUE(length(na.omit(unique(x))) == 1)) {

      stop("After converting user-missing values into NA, 'x' has only one unique value.", call. = FALSE)

    }

  }

  #-----------------------------------------
  # Unique values

  x.unique <- unique(na.omit(x))

  #-----------------------------------------
  # Number of observations

  x.length <- length(x)

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Input check 'x'
    if (isTRUE(is.numeric(x))) {

      if (isTRUE(any(na.omit(x) %% 1L != 0L))) {

        stop("Please specify a vector with integer values, a character vector or a factor for the argument 'x'.",
             call. = FALSE)

      }

    }

    #......
    # Input check 'x': Zero variance
    if (isTRUE(length(na.omit(unique(x))) == 1)) {

      stop("Variable specified in 'x' havs only one unique value.", call. = FALSE)

    }


    #......
    # Input check 'ref'
    if (isTRUE(!is.null(ref))) {

      if (isTRUE(!ref %in% x)) {

        stop("The reference category specified in 'ref' was not found in 'x'.", call. = FALSE)

      }

    }

    #......
    # Input check 'names'
    if (isTRUE(!is.character(names))) {

      stop("Please specify a character string or character vector for the argument 'names'.", call. = FALSE)

    }

    #......
    # Input check 'names'
    if (isTRUE(length(names) > 1L)) {

      if (isTRUE(length(names) != (length(x.unique) - 1L))) {

        stop("The length of the vector specified in 'names' does not match with the number of unique values minus one.",
             call. = FALSE)

      }

    }

  }

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # Create dummy matrix

  object <- matrix(0L, nrow = x.length, ncol = (length(x.unique) - 1L))

  #-----------------------------------------
  # Reference category

  # By default, the last category is the reference
  if (isTRUE(is.null(ref))) {

    if (isTRUE(is.numeric(x) || is.character(x))) {

      ref <- sort(x.unique)[length(x.unique)]

    }

    if (isTRUE(is.factor(x))) {

      ref <- levels(x)[length(levels(x))]

    }

  }

  #-----------------------------------------
  # Categories

  colnames(object) <- sort(x.unique[!x.unique %in% ref])

  #-----------------------------------------
  # Missing values

  if (isTRUE(any(is.na(x)))) {

    object[is.na(x), ] <- NA

  }

  #-----------------------------------------
  # Dummy coding

  for (i in colnames(object)) {

    object[which(x == i), i] <- 1L

  }

  #-----------------------------------------
  # Variable names

  if (isTRUE(length(names) == ncol(object))) {

    colnames(object) <- names

  } else {

    colnames(object) <- paste0(names, colnames(object))

  }

  ####################################################################################
  # Return object

  return(object)

}
