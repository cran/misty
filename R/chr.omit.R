#' Omit Strings
#'
#' This function omits user-specified values or strings from a numeric vector,
#' character vector or factor.
#'
#' @param x       a numeric vector, character vector or factor.
#' @param omit    a numeric vector or character vector indicating values or
#'                strings to be omitted
#'                from the vector \code{x}, the default setting is the empty
#'                strings \code{""}.
#' @param na.omit logical: if \code{TRUE}, missing values (\code{NA}) are also
#'                omitted from the vector.
#' @param check   logical: if \code{TRUE} (default), argument specification is
#'                checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{chr.gsub}}, \code{\link{chr.trim}}
#'
#' @return
#' Returns a numeric vector, character vector or factor with values or strings
#' specified in \code{omit} omitted from the vector specified in \code{x}.
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Charater vector
#' x.chr <- c("a", "", "c", NA, "", "d", "e", NA)
#'
#' # Example 1: Omit character string ""
#' chr.omit(x.chr)
#'
#' # Example 2: Omit character string "" and missing values (NA)
#' chr.omit(x.chr, na.omit = TRUE)
#'
#' # Example 3: Omit character string "c" and "e"
#' chr.omit(x.chr, omit = c("c", "e"))
#'
#' # Example 4: Omit character string "c", "e", and missing values (NA)
#' chr.omit(x.chr, omit = c("c", "e"), na.omit = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Numeric vector
#' x.num <- c(1, 2, NA, 3, 4, 5, NA)
#'
#' # Example 5: Omit values 2 and 4
#' chr.omit(x.num, omit = c(2, 4))
#'
#' # Example 6: Omit values 2, 4, and missing values (NA)
#' chr.omit(x.num, omit = c(2, 4), na.omit = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Factor
#' x.factor <- factor(letters[1:10])
#'
#' # Example 7: Omit factor levels "a", "c", "e", and "g"
#' chr.omit(x.factor, omit = c("a", "c", "e", "g"))
chr.omit <- function(x, omit = "", na.omit = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a numeric vector, character vector or factor for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'omit': Values in 'x'?
    na.x <- vapply(omit, function(y) !y %in% x, FUN.VALUE = logical(1))
    if (isTRUE(any(na.x))) { warning(paste0("Values specified in the argument 'omit' were not found in 'x': ", paste(omit[na.x], collapse = ", ")), call. = FALSE) }

    # Check input 'na.omit'
    if (isTRUE(!is.logical(na.omit))) { stop("Please specify TRUE or FALSE for the argument 'na.omit'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Omit NA ####

  if (isTRUE(na.omit)) {

    x <- na.omit(x)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Omit values or strings ####

  object <- x[which(!x %in% omit)]

  # Omit factor levels
  if (is.factor(object)) {

    object <- factor(object, levels = chr.omit(levels(object), omit = omit, check = FALSE))

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}
