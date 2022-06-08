#' Multiple Pattern Matching And Replacements
#'
#' This function is a multiple global string replacement wrapper that allows access
#' to multiple methods of specifying matches and replacements.
#'
#' @param pattern     a character vector with character strings to be matched.
#' @param replacement a character vector equal in length to \code{pattern} or of
#'                    length one which are a replacement for matched patterns.
#' @param x           a character vector where matches and replacements are sought.
#' @param recycle     logical: if \code{TRUE}, replacement is recycled if lengths differ.
#' @param ...         additional arguments to pass to the \code{regexpr} or \code{sub}
#'                    function.
#'
#' @author
#' Mark Ewing
#'
#' @seealso
#' \code{\link{chr.omit}}, \code{\link{chr.trim}}
#'
#' @references
#' Mark Ewing (2019). \emph{mgsub: Safe, Multiple, Simultaneous String Substitution}.
#' R package version 1.7.1. https://CRAN.R-project.org/package=mgsub
#'
#' @return
#' Return a character vector of the same length and with the same attributes as
#' \code{x} (after possible coercion to character).
#'
#' @note
#' This function was adapted from the \code{mgsub()} function in the \pkg{mgsub}
#' package by Mark Ewing (2019).
#'
#' @export
#'
#' @examples
#' string <- c("hey ho, let's go!")
#' chr.gsub(c("hey", "ho"), c("ho", "hey"), string)
#'
#' string <- "they don't understand the value of what they seek."
#' chr.gsub(c("the", "they"), c("a", "we"), string)
#'
#' string <- c("hey ho, let's go!")
#' chr.gsub(c("hey", "ho"), "yo", string, recycle = TRUE)
#'
#' string <- "Dopazamine is not the same as dopachloride or dopastriamine, yet is still fake."
#' chr.gsub(c("[Dd]opa([^ ]*?mine)","fake"), c("Meta\\1","real"), string)
chr.gsub <- function(pattern, replacement, x, recycle = FALSE, ...) {

  ####################################################################################
  # Data

  #---------------------
  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  ####################################################################################
  # Input Check

  #---------------------
  # All elements missing
  if (isTRUE(all(is.na(x)))) {

    return(x)

  }

  #---------------------
  # Logical vector with TRUE = not missing
  sna <- !is.na(x)

  #......
  # Check input 'recycle'
  if (isTRUE(!is.logical(recycle))) { stop("Please specify TRUE or FALSE for the argument 'recycle'.", call. = FALSE) }

  #......
  # Check if arguments 'argument' and 'replacement' have the same length
  if (isTRUE(!recycle & length(pattern) != length(replacement))) { stop("Pattern and replacement vectors must be the same length if recycle = FALSE.", call. = FALSE) }

  ####################################################################################
  # Functions

  #----------------------------------------
  # Fast escape replace
  #
  # Fast escape function for limited case where only one pattern
  # provided actually matches anything
  #
  # Argument string: a character vector where replacements are sought
  # Argument pattern: a character string to be matched in the given character vector
  # Argument replacement: Character string equal in length to pattern or of length
  #                       one which are a replacement for matched pattern.
  # Argument ...: arguments to pass to gsub()
  fastReplace <- function(string, pattern, replacement, ...) {

    for(i in seq_along(pattern)) {

      string <- gsub(pattern[i], replacement[i], string, ...)

    }

    return(string)

  }

  #----------------------------------------
  # Filter overlaps from matches
  #
  # Helper function used to identify which results from gregexpr()
  # overlap other matches and filter out shorter, overlapped results
  #
  # Argument x: a matrix of gregexpr() results, 4 columns, index of column matched,
  #             start of match, length of match, end of match. Produced exclusively from
  #             a worker function in chr.gsub
  filterOverlap <- function(x) {

    for(i in nrow(x):2L) {

      s <- x[i, 2L]
      ps <- x[1L:(i - 1L), 2L]
      e <- x[i, 4]
      pe <- x[1L:(i - 1L), 4L]

      if(any(ps <= s & pe >= s)){

        x <- x[-i, ]
        next

      }

      if(any(ps <= e & pe >= e)) {

        x <- x[-i,]

        next

      }

    }

    return(matrix(x, ncol = 4L))

  }

  #----------------------------------------
  # Get all matches
  #
  # Helper function to be used in a loop to check each pattern
  # provided for matches
  #
  # Argument string: a character vector where replacements are sought
  # Argument pattern: a character string to be matched in the given character vector
  # Argument i: an iterator provided by a looping function
  # Argument ...: arguments to pass to gregexpr()
  getMatches <- function(string ,pattern, i, ...){

    tmp <- gregexpr(pattern[i], string,...)
    start <- tmp[[1L]]
    length <- attr(tmp[[1L]], "match.length")
    return(matrix(cbind(i, start, length, start + length - 1L), ncol = 4L))

  }

  ###################
  # chr.gsub() worker
  #
  # The hard worker doing everything for chr.gsub()
  #
  # Argument string: a character vector where replacements are sought
  # Argument pattern: a character string to be matched in the given character vector
  # Argument replacement: a character string equal in length to pattern or of length
  #                       one which are a replacement for matched pattern.
  # Argument ...: arguments to pass to regexpr family
  worker <- function(string, pattern, replacement,...){

    x0 <- do.call(rbind, lapply(seq_along(pattern), getMatches, string = string, pattern = pattern, ...))
    x0 <- matrix(x0[x0[, 2] != -1L, ], ncol = 4L)

    uid <- unique(x0[, 1L])
    if(nrow(x0) == 0L) {

      return(string)

    }

    if(length(unique(x0[, 1])) == 1L) {

      return(fastReplace(string, pattern[uid], replacement[uid], ...))

    }

    if(nrow(x0) > 1L) {

      x <- x0[order(x0[, 3L], decreasing = TRUE), ]
      x <- filterOverlap(x)
      uid <- unique(x[, 1L])

      if(length(uid) == 1L) {

        return(fastReplace(string, pattern[uid], replacement[uid], ...))

      }

      x <- x[order(x[, 2L]), ]
    }

    for(i in nrow(x):1L){

      s <- x[i, 2L]
      e <- x[i, 4L]
      p <- pattern[x[i, 1L]]
      r <- replacement[x[i, 1L]]

      pre <- if(s > 1) { substr(string, 1L, s - 1L) } else { "" }
      r0 <- sub(p,r,substr(string, s, e), ...)
      end <- if(e < nchar(string)) { substr(string, e + 1, nchar(string)) } else { "" }
      string <- paste0(pre, r0, end)

    }

    return(string)

  }

  ####################################################################################
  # Main Function

  if (isTRUE(length(replacement) > length(pattern))) {

    warning("More replacements than search strings provided, some will be dropped.",
            call. = FALSE)

    replacement <- replacement[seq_along(pattern)]

  }

  if (isTRUE(recycle && length(pattern) != length(replacement))) {

    lp <- length(pattern)
    lr <- length(replacement)
    replacement <- rep(replacement, ceiling(lp/lr))[seq_along(pattern)]

  }

  result <- vapply(X = x[sna], FUN = worker, FUN.VALUE = c(""),
                  USE.NAMES = FALSE, pattern = pattern, replacement = replacement, ...)

  x[sna] <- result

  return(x)

}
