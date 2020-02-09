#' Omit Strings
#'
#' This function omits user-specified values or strings from a numeric vector, character vector or factor.
#'
#' @param x           a numeric vector, character vector or factor.
#' @param omit        a numeric vector or character vector indicating values or strings to be omitted
#'                    from the vector \code{x}, the default setting is the empty strings \code{""}.
#' @param na.omit     logical: if \code{TRUE}, missing values (\code{NA}) are also omitted from the
#'                    vector.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @return
#' Returns a numeric vector, character vector or factor with values or strings
#' specified in \code{omit} omitted from the vector specified in \code{x}.
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Charater vector
#' x.chr <- c("a", "", "c", NA, "", "d", "e", NA)
#'
#' # Omit character string ""
#' stromit(x.chr)
#'
#' # Omit character string "" and missing values (NA)
#' stromit(x.chr, na.omit = TRUE)
#'
#' # Omit character string "c" and "e"
#' stromit(x.chr, omit = c("c", "e"))
#'
#' # Omit character string "c", "e", and missing values (NA)
#' stromit(x.chr, omit = c("c", "e"), na.omit = TRUE)
#'
#' #--------------------------------------
#' # Numeric vector
#' x.num <- c(1, 2, NA, 3, 4, 5, NA)
#'
#' # Omit values 2 and 4
#' stromit(x.num, omit = c(2, 4))
#'
#' # Omit values 2, 4, and missing values (NA)
#' stromit(x.num, omit = c(2, 4), na.omit = TRUE)
#'
#' #--------------------------------------
#' # Factor
#' x.factor <- factor(letters[1:10])
#'
#' # Omit factor levels "a", "c", "e", and "g"
#' stromit(x.factor, omit = c("a", "c", "e", "g"))
stromit <- function(x, omit = "", na.omit = FALSE, check = TRUE) {

  ####################################################################################
  # Input check

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a numeric vector, character vector or factor for the argument 'x'", call. = FALSE)

  }

  #.............
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #.............
    # Check input 'x': Vector?
    if (!is.null(dim(x))) {

      stop("Please specify a vector for the argument 'x'.", call. = FALSE)

    }

    #.............
    # Check input 'omit': Values in 'x'?
    na.x <- vapply(omit, function(y) !y %in% x, FUN.VALUE = logical(1))
    if (any(na.x)) {

      warning(paste0("Values specified in the argument 'omit' were not found in 'x': ",
                     paste(omit[na.x], collapse = ", ")), call. = FALSE)
    }

    #.............
    # Check input 'na.omit'
    if (isFALSE(isTRUE(na.omit) || isFALSE(na.omit))) {

      stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  #-----------------------------------
  # Omit NA

  if (isTRUE(na.omit)) {

    x <- na.omit(x)

  }

  #-----------------------------------
  # Omit values or strings

  object <- x[which(!x %in% omit)]

  # Omit factor levels
  if (is.factor(object)) {

    object <- factor(object, levels = stromit(levels(object), omit = omit, check = FALSE))

  }

  ####################################################################################
  # Return object

  return(object)

}
