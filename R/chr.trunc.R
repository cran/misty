#' Truncate a Character Vector to a Maximum Width
#'
#' This function truncates a character vector, so that the number of characters
#' of each element of the character vector is always less than or equal to the
#' width specified in the argument \code{width}.
#'
#' @param x        a character vector or factor. Note that factors are converted
#'                 into a character vector.
#' @param width    a numeric value indicating the maximum width of the character
#'                 strings in the vector.
#' @param side     a character string indicating the location of the ellipsis,
#'                 i.e. \code{"right"} (default) for the right side, \code{"left"}
#'                 for the left side, and \code{"center"} for center of the
#'                 character strings in the vector
#' @param ellipsis a character string indicating the content of the ellipsis,
#'                 i.e., \code{"..."} by default. Note that the default setting
#'                 switches to \code{".."} when \code{width = 3}, \code{"."} when
#'                 \code{width = 2}, and \code{""} when \code{width = 1}.
#' @param check    logical: if \code{TRUE} (default), argument specification is
#'                 checked.
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{chr.color}}, \code{\link{chr.grep}}, \code{\link{chr.grepl}},
#' \code{\link{chr.gsub}}, \code{\link{chr.omit}}, \code{\link{chr.trim}}
#'
#' @references
#' Wickham H (2023). \emph{stringr: Simple, Consistent Wrappers for Common String Operations}.
#' R package version 1.5.1, https://CRAN.R-project.org/package=stringr
#'
#' @return
#' Returns a truncated character vector.
#'
#' @note
#' This function was adapted from the \code{str_trunc()} function in the \pkg{stringr}
#' package by Hadley Wickham (2023).
#'
#' @export
#'
#' @examples
#' # Example 1: Truncate at the right side with a max. of 10 characters
#' chr.trunc(row.names(mtcars), width = 10)
#'
#' # Example 2: Truncate at the left side with a max. of 10 characters
#' chr.trunc(row.names(mtcars), width = 10, side = "left")
#'
#' # Example 3: Truncate without ellipses
#' chr.trunc(row.names(mtcars), width = 10, ellipsis = "")
chr.trunc <- function (x, width, side = c("right", "left", "center"), ellipsis = "...", check = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a character vector or factor for the argument 'x'", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Check if input 'x' is a character vector or factor
  if (isTRUE(!is.character(x) && !is.factor(x))) { stop("Please specify a character vector or factor for the argument 'x'", call. = FALSE) }

  # Check if input 'width' is missing
  if (isTRUE(missing(width))) { stop("Please specify a numeric value for the argument 'width'", call. = FALSE) }

  # Check if input 'width' is NULL
  if (isTRUE(is.null(width))) { stop("Input specified for the argument 'width' is NULL.", call. = FALSE) }

  # Convert 'x' into a character vector
  if (isTRUE(is.factor(x))) { x <- as.character(x) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  .check.input(numeric = list(width = 1L),
               character = list(ellipsis = 1L),
               s.character = list(side = c("right", "left", "center")), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(isTRUE(check))) {

    # Check input 'width'
    if (isTRUE(width <= 0)) { stop("Please specify a numeric value greater than zero for the argument 'width'", call. = FALSE) }

    # Check input 'ellipsis'
    if (isTRUE(ellipsis != "..." && (width - nchar(ellipsis) < 0L))) { stop("The difference between 'width' and the number of characters in 'ellipsis' is negative.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'side' Argument ####

  side <- ifelse(all(c("right", "left", "center") %in% side), "right", side)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## 'ellipsis' Argument ####

  if (isTRUE(ellipsis == "..." && width <= 3L)) { ellipsis <- switch(as.character(width), "3" = "..", "2" = ".", "1" = "") }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  trunc.x <- !is.na(x) & nchar(x) > width

  x[trunc.x] <- list(nchar.x = nchar(x), width.e = width - nchar(ellipsis)) |>
                  (\(y) switch(side,
                              right = paste0(substr(x[trunc.x], 1L, y$width.e), ellipsis),
                               left = paste0(ellipsis, substr(x[trunc.x], y$nchar.x[trunc.x] - y$width.e + 1L, y$nchar.x[trunc.x])),
                             center = paste0(substr(x[trunc.x], 1L, ceiling(y$width.e / 2L)), ellipsis, substr(x[trunc.x], y$nchar.x[trunc.x] - floor(y$width.e / 2L) + 1L, y$nchar.x[trunc.x]))))()

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  return(x)

}

#_______________________________________________________________________________
