#' Multiple Pattern Matching
#'
#' This function searches for matches to the character vector specified in
#' \code{pattern} within each element of the character vector \code{x}.
#'
#' @param pattern     a character vector with character strings to be matched.
#' @param x           a character vector where matches are sought.
#' @param ignore.case logical: if \code{FALSE} (default), the pattern matching
#'                    is case sensitive and if \code{TRUE}, case is ignored during
#'                    matching.
#' @param perl        logical: if \code{TRUE} Perl-compatible regexps are used.
#' @param value       logical: if \code{FALSE} (default), a vector containing the
#'                    (integer) indices of the matches determined by grep is
#'                    returned, and if \code{TRUE}, a vector containing the
#'                    matching elements themselves is returned.
#' @param fixed       logical: if \code{TRUE}, pattern is a string to be matched
#'                    as is. Overrides all conflicting arguments.
#' @param useBytes    logical: if \code{TRUE}, the matching is done byte-by-byte
#'                    rather than character-by-character. See ‘Details’.
#' @param invert      logical: if \code{TRUE}, function returns indices or values
#'                    for elements that do not match.
#' @param check       logical: if \code{TRUE} (default), argument specification
#'                    is checked.
#'
#' @author
#' Takuya Yanagida
#'
#' @name chr.grep
#'
#' @seealso
#' \code{\link{chr.color}}, \code{\link{chr.grepl}}, \code{\link{chr.gsub}},
#' \code{\link{chr.omit}}, \code{\link{chr.trim}}, \code{\link{chr.trunc}}
#'
#' @references
#' Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The New S Language}.
#' Wadsworth & Brooks/Cole
#'
#' @return
#' Returns a integer vector with the indices of the mathces when \code{value = FALSE},
#' character vector containing the matching elements when \code{value = TRUE}, or
#' a logical vector when using the \code{chr.grepl} function.
#'
#' @export
#'
#' @examples
#' chr.vector <- c("James", "Mary", "Michael", "Patricia", "Robert", "Jennifer")
#'
#' # Example 1: Indices of matching elements
#' chr.grep(c("am", "er"), chr.vector)
#'
#' # Example 2: Values of matching elements
#' chr.grep(c("am", "er"), chr.vector, value = TRUE)
#'
#' # Example 3: Matching element?
#' chr.grepl(c("am", "er"), chr.vector)
chr.grep <- function(pattern, x, ignore.case = FALSE, perl = FALSE, value = FALSE,
                     fixed = FALSE, useBytes = FALSE, invert = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'pattern' is missing or NULL
  if (isTRUE(missing(pattern) || is.null(pattern))) { stop("Please specify a character vector for the argument 'pattern'", call. = FALSE) }

  # Check if input 'x' is missing or NULL
  if (isTRUE(missing(x) || is.null(x))) { stop("Please specify a character vector for the argument 'x'", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("ignore.case", "perl", "value", "fixed", "useBytes", "invert"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Matching Elements, invert = FALSE ####

  if (isTRUE(!invert)) {

    #...................
    ### Character String to be Matched ####

    if (isTRUE(length(pattern) == 1L)) {

      object <- grep(pattern, x, ignore.case = ignore.case, perl = perl, value = FALSE, fixed = fixed, useBytes = useBytes, invert = FALSE)


    #...................
    ### Character Vector to be Matched ####

    } else {

      object <- sort(unique(unlist(sapply(pattern, function(y) grep(y, x, ignore.case = ignore.case, perl = perl, value = FALSE, fixed = fixed, useBytes = useBytes, invert = FALSE)))))

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Not Matching Elements, invert = TRUE ####

  } else {

    #...................
    ### Character String to be Matched ####

    if (isTRUE(length(pattern) == 1L)) {

      object <- grep(pattern, x, ignore.case = ignore.case, perl = perl, value = FALSE, fixed = fixed, useBytes = useBytes, invert = TRUE)


    #...................
    ### Character Vector to be Matched ####

    } else {

      object <- as.numeric(names(which(table(unlist(sapply(pattern, function(y) grep(y, x, ignore.case = ignore.case, perl = perl, value = FALSE,
                                                           fixed = fixed, useBytes = useBytes, invert = TRUE)))) == length(pattern))))

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Matching Elements, value = TRUE ####

  if (isTRUE(value)) { object <- x[object] }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
#_______________________________________________________________________________

#' @rdname chr.grepl
chr.grepl <- function(pattern, x, ignore.case = FALSE, perl = FALSE, fixed = FALSE,
                      useBytes = FALSE, check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'pattern' is missing
  if (isTRUE(missing(pattern))) { stop("Please specify a character vector for the argument 'pattern'", call. = FALSE) }

  # Check if input 'pattern' is NULL
  if (isTRUE(is.null(pattern))) { stop("Input specified for the argument 'pattern' is NULL.", call. = FALSE) }

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a character vector for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  .check.input(logical = c("ignore.case", "perl", "fixed", "useBytes"), envir = environment(), input.check = check)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Character String to be Matched ####

  if (isTRUE(length(pattern) == 1L)) {

    object <- grepl(pattern, x, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Character Vector to be Matched ####

  } else {

    if (isTRUE(length(x) == 1L)) {

      object <- unname(sapply(pattern, function(y) grepl(y, x, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)))

    } else {

      object <- apply(sapply(pattern, function(y) grepl(y, x, ignore.case = ignore.case, perl = perl, fixed = fixed, useBytes = useBytes)), 1L, any)

    }

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(object)

}

#_______________________________________________________________________________
