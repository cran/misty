#' Replace Missing Values With User-Specified Values
#'
#' This function replaces \code{NA} in a vector, factor, matrix or data frame with
#' user-specified values in the argument \code{value}.
#'
#' @param x           a vector, factor, matrix or data frame.
#' @param value       a numeric value or character string with which \code{NA} is
#'                    replaced.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' \code{\link{as.na}}, \code{\link{na.auxiliary}}, \code{\link{na.coverage}},
#' \code{\link{na.descript}}, \code{\link{na.indicator}}, \code{\link{na.pattern}},
#' \code{\link{na.prop}}, \code{\link{na.test}}
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}.
#' Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns \code{x} with \code{NA} replaced with the numeric value or character
#' string specified in \code{value}.
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Numeric vector
#' x.num <- c(1, 3, NA, 4, 5)
#'
#' # Replace NA with 2
#' na.as(x.num, value = 2)
#'
#' #--------------------------------------
#' # Character vector
#' x.chr <- c("a", NA, "c", "d", "e")
#'
#' # Replace NA with "b"
#' na.as(x.chr, value = "b")
#'
#' #--------------------------------------
#' # Factor
#' x.factor <- factor(c("a", "a", NA, NA, "c", "c"))
#'
#' # Replace NA with "b"
#' na.as(x.factor, value = "b")
#'
#' #--------------------------------------
#' # Matrix
#' x.mat <- matrix(c(1, NA, 3, 4, 5, 6), ncol = 2)
#'
#' # Replace NA with 2
#' na.as(x.mat, value = 2)
#'
#' #--------------------------------------
#' # Data frame
#' x.df1 <- data.frame(x1 = c(NA, 2, 3),
#'                     x2 = c(2, NA, 3),
#'                     x3 = c(3, NA, 2), stringsAsFactors = FALSE)
#'
#' # Replace NA with -99
#' na.as(x.df1, value = -99)
#'
#' #--------------------------------------
#' # Recode value in data frame
#' x.df2 <- data.frame(x1 = c(1, 2, 30),
#'                     x2 = c(2, 1, 30),
#'                     x3 = c(30, 1, 2))
#'
#' # Replace 30 with NA and then replace NA with 3
#' na.as(x.df2, value = 3, as.na = 30)
na.as <- function(x, value, as.na = NULL, check = TRUE) {

  #-----------------------------------------------------------------------------------
  # Data

  #...............
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a vector, factor, matrix or data frame for the argument 'x'.",
         call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #...............
  # Check if input 'value' is missing
  if (isTRUE(missing(value))) {

    stop("Please specify a numeric value or character string for the argument 'value'.",
         call. = FALSE)

  }

  #...............
  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

  }

  #-----------------------------------------------------------------------------------
  # Input Check

  #...............
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #...............
    # Vector, factor, matrix or data frame for the argument 'x'?
    if (isTRUE(!is.atomic(x) && !is.factor(x) && !is.matrix(x) && !is.data.frame(x))) {

      stop("Please specifiy a vector, factor, matrix or data frame for the argument 'x'.",
           call. = FALSE)

    }

    #...............
    # Factor or Vector
    if (isTRUE(is.null(dim(x)))) {

      if (isTRUE(all(!is.na(x)))) {

        warning("There are no missing values (NA) in the vector or factor specified in 'x'.",
                call. = FALSE)

      }

    #...............
    # Matrix or data frame
    } else {

      if (isTRUE(all(apply(x, 2, function(y)  all(!is.na(y))))))  {

        warning("There are no missing values (NA) in the matrix or data frame specified in 'x'.",
                call. = FALSE)

      }

    }

    #...............
    # Check input 'value'
    if (isTRUE(length(value) != 1L)) {

      stop("Please specifiy a single value or character string for the argument 'value'.",
           call. = FALSE)

    }

  }

  #-----------------------------------------------------------------------------------
  # Main Function

  # Factor or Vector
  if (isTRUE(is.null(dim(x)))) {

    # Factor
    if (isTRUE(is.factor(x))) {

      # Factor levels
      f.levels <- sort(unique(as.numeric(x)))

      f.value <- length(f.levels) + 1L
      f.levels <- c(f.levels, f.value)

      # Factor labels
      f.labels <- c(levels(x), value)

      object <- factor(ifelse(is.na(x), f.value, x), levels = f.levels, labels = f.labels)

    # Vector
    } else {

      object <- ifelse(is.na(x), value, x)

    }

  # Matrix or data frame
  } else {

    # Matrix
    if (isTRUE(is.matrix(x))) {

        object <- apply(x, 2, na.as, value = value, check = FALSE)

    }

    # Data frame
    if (isTRUE(is.data.frame(x))) {

      object <- data.frame(lapply(x, na.as, value = value, check = FALSE),
                           check.names = FALSE, fix.empty.names = FALSE)

    }

  }

  #-----------------------------------------------------------------------------------
  # Return object and output

  return(object)

}
