#' Replace User-Specified Values With Missing Values
#'
#' This function replaces user-spefied values in the argument \code{na} in a vector, factor, matrix or
#' data frame with \code{NA}.
#'
#' @param x         a vector, factor, matrix or data frame.
#' @param na        a vector indicating values or characters to replace with \code{NA}.
#' @param check     logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{na.as}}, \code{\link{na.auxiliary}}, \code{\link{na.coverage}}, \code{\link{na.descript}},
#' \code{\link{na.indicator}}, \code{\link{na.pattern}}, \code{\link{na.prop}}.
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}. Wadsworth & Brooks/Cole.
#'
#' @return
#' Returns \code{x} with values specified in \code{na} replaced with \code{NA}.
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Numeric vector
#' x.num <- c(1, 3, 2, 4, 5)
#'
#' # Replace 2 with NA
#' as.na(x.num, na = 2)
#'
#' # Replace 2, 3, and 4 with NA
#' as.na(x.num, na = c(2, 3, 4))
#'
#' #--------------------------------------
#' # Character vector
#' x.chr <- c("a", "b", "c", "d", "e")
#'
#' # Replace "b" with NA
#' as.na(x.chr, na = "b")
#'
#' # Replace "b", "c", and "d" with NA
#' as.na(x.chr, na = c("b", "c", "d"))
#'
#' #--------------------------------------
#' # Factor
#' x.factor <- factor(c("a", "a", "b", "b", "c", "c"))
#'
#' # Replace "b" with NA
#' as.na(x.factor, na = "b")
#'
#' # Replace "b" and "c" with NA
#' as.na(x.factor, na = c("b", "c"))
#'
#' #--------------------------------------
#' # Matrix
#' x.mat <- matrix(1:20, ncol = 4)
#'
#' # Replace 8 with NA
#' as.na(x.mat, na = 8)
#'
#' # Replace 8, 14, and 20 with NA
#' as.na(x.mat, na = c(8, 14, 20))
#'
#' #--------------------------------------
#' # Data frame
#' x.df <- data.frame(x1 = c(1, 2, 3),
#'                    x2 = c(2, 1, 3),
#'                    x3 = c(3, 1, 2))
#'
#' # Replace 1 with NA
#' as.na(x.df, na = 1)
#'
#' # Replace 1 and 3 with NA
#' as.na(x.df, na = c(1, 3))
as.na <- function(x, na, check = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #.........................................

  if (isTRUE(check)) {

    #......
    # Check if input 'x' missing
    if (missing(x)) {

      stop("Please specify a vector, factor, matrix or data frame for the argument 'x'",
           call. = FALSE)

    }

    #......
    # Vector, factor, matrix or data frame for the argument 'x'?
    if (!is.vector(x) && !is.factor(x) && !is.matrix(x) && !is.data.frame(x)) {

      stop("Please specifiy a vector, factor, matrix or data frame for the argument 'x'.",
           call. = FALSE)

    }

    #......
    # Check if input 'na' missing
    if (missing(na)) {

      stop("Please specify a numeric vector or character vector for the argument 'na'", call. = FALSE)

    }

    #......
    # Check input 'na': Values in 'na'?

    # Factor or Vector
    if (is.null(dim(x))) {

      na.x <- !na %in% x

    }

    # Matrix or data frame
    if (is.matrix(x) || is.data.frame(x)) {

      na.x <- sapply(as.character(na), function(y) !y %in% misty::trim(apply(as.matrix(x), 2, as.character)))

    }

    if (any(na.x)) {

      warning(paste0("Values specified in the argument 'na' were not found in 'x': ",
                      paste(na[na.x], collapse = ", ")), call. = FALSE)
    }

  }

  ####################################################################################
  # Main Function

  #---------------------------------
  # Factor or Vector
  if (is.null(dim(x))) {

    #..........
    # Factor
    if (is.factor(x)) {

      f.levels <- sort(unique(as.numeric(x)))
      f.labels <- levels(x)

      if (any(na %in% f.labels)) {

        f.levels <- f.levels[-which(f.labels %in% na)]
        f.labels <- f.labels[-which(f.labels %in% na)]

      }

      object <- factor(ifelse(x %in% na, NA, x), levels = f.levels, labels = f.labels)

    #..........
    # Vector
    } else {

      object <- ifelse(x %in% na, NA, x)
      names(object) <- names(x)

    }

  #---------------------------------
  # Matrix or data frame
  } else {

    #..........
    # Matrix
    if (is.matrix(x)) {

      object <- apply(x, 2, as.na, na = na, check = FALSE)

    }

    #..........
    # Data frame
    if (is.data.frame(x)) {

      object <- data.frame(lapply(x, as.na, na = na, check = FALSE))

    }

  }

  ####################################################################################
  # Return object

  return(object)

}
