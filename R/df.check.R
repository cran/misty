#' Data Check
#'
#' This function is a wrapper around the functions \code{dim} for the number of
#' rows and columns, \code{names} for the variable names, \code{df.head} for the
#' first rows, and \code{df.tail} for the last rows of a data frame.
#'
#' @param data          a data frame.
#' @param print         a character string or character vector indicating which
#'                      results to show on the console, i.e., \code{"dim"}, for
#'                      the number of rows and number of columns, \code{"names"}
#'                      for the variable names, \code{"head"} for the first rows
#'                      of the data frame, and \code{"tail"} for the last rows
#'                      of the data frame.
#' @param n             a numeric value indicating the number of rows to be
#'                      printed on the console.
#' @param digits        a numeric value indicating the maximum number of decimal
#'                      places to be used.
#' @param width         a numeric value indicating the maximum width of the
#'                      character strings in the vector.
#' @param row.names     logical: if \code{TRUE}, row names of the data frame are
#'                      printed on the console.
#' @param row.names.col a character string indicating the text color for the row
#'                      names, see \code{color} argument of the \code{\link{chr.color}}
#'                      function.
#' @param message       logical: if \code{TRUE}, number of remaining rows and
#'                      columns are printed on the console.
#' @param message.col   a character string indicating the text color for the
#'                      number of remaining rows and columns printed on the
#'                      console, see \code{color} argument of the
#'                      \code{\link{chr.color}} function.
#' @param check         logical: if \code{TRUE} (default), argument specification
#'                      is checked.
#' @param output        logical: if \code{TRUE} (default), output is shown on the
#'                      console.
#'
#' @details
#' Note that this function only provides a basic data check suitable for checking
#' a data frame after importing data into R and is not designed to offer a thorough
#' data check (e.g., identifying duplicate IDs or inconsistencies in the data).
#'
#' @author
#' Takuya Yanagida
#'
#' @seealso
#' \code{\link{df.head}}, \code{\link{df.tail}}
#'
#' @export
#'
#' @examples
#' # Example 1: Check data frame mtcars
#' df.check(mtcars)
df.check <- function(data, print = c("dim", "names", "head", "tail"),
                     n = 4, digits = 3, width = 20, row.names = TRUE, row.names.col = "gray2",
                     message = TRUE, message.col = "b.blue", check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(data))) { stop("Please specify a numeric vector for the argument 'data'", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(data))) { stop("Input specified for the argument 'data' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("row.names", "message"), numeric = list(n = 1L, width = 1L), m.character = list(print = c("dim", "names", "head", "tail")), args = "digits", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'n'
    if (isTRUE(n <= 0L)) { stop("Please specify a numeric value greater than zero for the argument 'n'", call. = FALSE) }

    # Check input 'width'
    if (isTRUE(width <= 0L)) { stop("Please specify a numeric value greater than zero for the argument 'width'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  n.print <- switch(as.character(n), "2" = " two ", "3" = " three ", "4" = " four ", "5" = " five ", "6" = " six ", "7" = " seven ", "8" = " eight ", "9" = " nine ",  "10" = " ten ")

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Dimensions ####

  data.dim <- format(data.frame(x = c("  No. of rows:    ", "  No. of columns: "), y = c(nrow(data), ncol(data))), justify = "right")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Variable Names ####

  for (i in seq_along(names(data))) {

    # Variable names
    data.names.temp <- names(data)

    # Length of variable names not divisible by i
    if (isTRUE((length(data.names.temp) / i) %% 1L != 0L)) {

      repeat {

        data.names.temp <- c(data.names.temp, "")

        if (isTRUE((length(data.names.temp) / i) %% 1L == 0L)) break

      }

    }


    # Variable print object
    data.names.print <- format(as.data.frame(matrix(sapply(data.names.temp, function(y) ifelse(y != "", shQuote(y), y)), nrow = i, byrow = TRUE)), justify = "left")

    if (isTRUE(max(apply(data.names.print, 1L, function(y) nchar(paste(y, collapse = " "))) + 3L) < getOption("width"))) break

  }

  # Format
  data.names.print[, 1L] <- paste("  ", data.names.print[, 1L])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## First and Last Rows ####

  # First and last rows
  data.head <- misty::df.head(data, n = n, digits = digits, width = width, row.names = row.names, row.names.col = row.names.col, message = message, message.col = message.col, check = FALSE, output = FALSE)$result
  data.tail <- misty::df.tail(data, n = n, digits = digits, width = width, row.names = row.names, row.names.col = row.names.col, message = message, message.col = message.col,check = FALSE, output = FALSE)$result

  # Format
  data.head$df[, 1L] <- paste("  ", data.head$df[, 1L])
  data.head$row.col <- paste("  ", data.head$row.col)

  data.tail$df[, 1L] <- paste("  ", data.tail$df[, 1L])
  data.tail$row.col <- paste("  ", data.tail$row.col)

  #_____________________________________________________________________________
  #
  # Print ----------------------------------------------------------------------

  cat(" Data Check\n\n")

  #...................
  ### Dimensions ####

  if (isTRUE("dim" %in% print)) { write.table(data.dim, quote = FALSE, col.names = FALSE, row.names = FALSE) }

  #...................
  ### Variable names ####

  if (isTRUE("names" %in% print)) {

    if (isTRUE("dim" %in% print)) { cat("\n") }

    cat("  Variable names\n")

    write.table(data.names.print, quote = FALSE, col.names = FALSE, row.names = FALSE)

  }

  # Number of rows to be printed smaller than the number of rows of the data frame
  if (isTRUE(n < nrow(data))) {
    #...................
    ### First Rows ####

    if (isTRUE("head" %in% print)) {

      if (isTRUE(any(c("dim", "names") %in% print))) { cat("\n") }

      if (isTRUE(n == 1L)) {

        cat("  First row\n")

      } else if (isTRUE(n <= 10L)) {

        cat(paste0("  First", n.print, "rows\n"))

      } else {

        cat(paste0("  First ", n, " rows\n"))

      }

      # Print first rows
      write.table(data.head$df, quote = FALSE, col.names = FALSE, row.names = FALSE)

      # Number of remaining rows and columns
      if (isTRUE(message)) { write.table(data.head$row.col, quote = FALSE, col.names = FALSE, row.names = FALSE) }

    }

    #...................
    ### Last Rows ####

    if (isTRUE("tail" %in% print)) {

      if (isTRUE(any(c("dim", "names", "head") %in% print))) { cat("\n") }

      if (isTRUE(n == 1L)) {

        cat("  Last row\n")

      } else if (isTRUE(n <= 10L)) {

        cat(paste0("  Last", n.print, "rows\n"))

      } else {

        cat(paste0("  Last ", n, " rows\n"))

      }

      # Print last rows
      write.table(data.tail$df, quote = FALSE, col.names = FALSE, row.names = FALSE)

      # Number of remaining rows and columns
      if (isTRUE(message)) { write.table(data.tail$row.col, quote = FALSE, col.names = FALSE, row.names = FALSE) }

    }

  # Number of rows to be printed larger equal than the number of rows of the data frame
  } else if (isTRUE(any(c("head", "tail") %in% print))) {

    if (isTRUE(any(c("dim", "names") %in% print))) { cat("\n") }

    cat("  Data frame\n")
    write.table(data.head$df, quote = FALSE, col.names = FALSE, row.names = FALSE)

  }

}

#_______________________________________________________________________________
