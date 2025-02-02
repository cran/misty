#' Print the First and Last Rows of a Data Frame
#'
#' The function \code{df.head} prints the first rows of a data frame and the
#' function \code{df.tail} prints the last rows of a data frame and prints as
#' many columns as fit on the console supplemented by a summary of the remaining
#' rows and columns.
#'
#' @param x             a data frame.
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
#' @param message       logical: if \code{TRUE}, number of remaining rows and columns
#'                      are printed on the console.
#' @param message.col   a character string indicating the text color for the
#'                      number of remaining rows and columns printed on the
#'                      console, see \code{color} argument of the
#'                      \code{\link{chr.color}} function.
#' @param check         logical: if \code{TRUE} (default), argument specification
#'                      is checked.
#' @param output        logical: if \code{TRUE} (default), output is shown on the
#'                      console.
#'
#' @author
#' Takuya Yanagida
#'
#' @name df.head
#'
#' @seealso
#' \code{\link{df.check}}, \code{\link{head}}, \code{\link{tail}}, \code{\link{freq}},
#' \code{\link{descript}}
#'
#' @return
#' Returns a list with following entries:
#'
#' \item{\code{df}}{data frame specified in \code{x} with the first or last \code{n}
#' rows of the data frame with as many columns as fit on the console}
#' \item{\code{row.col}}{character string indicating the remaining rows and columns}
#'
#' @export
#'
#' @examples
#' # Example 1: Print first and last six rows
#' df.head(mtcars)
#' df.tail(mtcars)
#'
#' # Example 2: Print first and last six rows without row names
#' df.head(mtcars, row.names = FALSE)
#' df.tail(mtcars, row.names = FALSE)
#'
#' # Example 3: Print first and last three rows with one max. number of decimal places
#' df.head(mtcars, n = 3, digits = 1)
#' df.head(mtcars, n = 3, digits = 1)
df.head <- function(x, n = 6, digits = 3, width = 20, row.names = TRUE, row.names.col = "gray2",
                    message = TRUE, message.col = "b.blue", check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(x) || !is.data.frame(x))) { stop("Please specify a data frame for the argument 'x'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("row.names", "message"), numeric = list(n = 1L, width = 1L), character = list(row.names.col = 1L, message.col = 1L), args = "digits", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'n'
    if (isTRUE(n <= 0L)) { stop("Please specify a numeric value greater than zero for the argument 'n'", call. = FALSE) }

    # Check input 'width'
    if (isTRUE(width <= 0L)) { stop("Please specify a numeric value greater than zero for the argument 'width'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Format Data Frame ####

  object <- sapply(x, function(y) {

    # Numeric variables
    if (isTRUE(is.numeric(y) && any(y %% 1L != 0L))) {

      # Number of digits > 'digits'
      if (isTRUE(any((nchar(as.character(y)) - nchar(as.integer(y))) |> (\(y) ifelse(y != 0L, y - 1L, y))() > digits))) {

        ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA)

      # Number of digits <= 'digits'
      } else {

        (max(nchar(as.character(y)) - nchar(as.integer(y))) - 1L) |> (\(w) ifelse(!is.na(y), formatC(y, digits = w, format = "f", zero.print = ifelse(w > 0L, paste0("0.", paste(rep(0, times = w), collapse = "")), "0")), NA))()

      }

    # Characters or factors
    } else if (isTRUE(is.character(y) || is.factor(y) && any(nchar(as.character(y)) > width))) {

      ifelse(!is.na(y), misty::chr.trunc(y, width = width, check = FALSE), y)

    } else {

      return(y)

    }

  }) |> (\(y) if (isTRUE(is.null(dim(y)))) { as.data.frame(t(y)) } else if (isTRUE(!is.data.frame(y))) { as.data.frame(y) } else { return(y) })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add Row Names ####

  if (isTRUE(row.names)) {

    object <- apply(rbind(names(object), object), 2L, format, justify = "right") |> (\(y) cbind(format(c("", row.names(x)), justify = "left"), y))()

  } else {

    object <- apply(rbind(names(object), object), 2L, format, justify = "right", trim = TRUE, width = NULL)

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Rows ####

  if (isTRUE(n < nrow(x))) {

    object <- object[seq_len(n + 1L), , drop = FALSE]

    if (isTRUE(row.names)) { object[, 1L] <- format(misty::chr.trim(object[, 1L]), justify = "left") }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Fit Console ####

  if (isTRUE(max(nchar(apply(object, 1L, function(y) paste(y, collapse = " ")))) > getOption("width"))) {

    object <- object[, seq_len(rev(which((cumsum(apply(object, 2L, function(y) max(nchar(y))) + 1L) - 0L) <= getOption("width")))[1L])]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Format Row Names ####

  if (isTRUE(row.names && is.null(getOption("knitr.in.progress")))) { object[, 1L] <- misty::chr.color(object[, 1L], color = row.names.col, check = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Message ####

  if (message) {

    # Rows
    if (isTRUE(n < nrow(x))) { row.col <- (nrow(x) - n) |> (\(y) paste0(y, " more ", ifelse(y == 1L, "row", "rows")))() } else { row.col <- NULL }

    # Columns
    if (isTRUE(ncol(object) < ifelse(row.names, ncol(x) + 1L, ncol(x)))) {

      if (isTRUE(!is.null(row.col))) {

        row.col <- paste0(row.col, " and ", (ifelse(row.names, ncol(x) + 1L, ncol(x)) - ncol(object)) |> (\(y) paste0(y, " more ", ifelse(y == 1L, "variable", "variables")))())

      } else {

        row.col <- (ifelse(row.names, ncol(x) + 1L, ncol(x)) - ncol(object)) |> (\(y) paste0(y, " more ", ifelse(y == 1L, "variable", "variables")))()

      }

    }

    # Format message
    if (isTRUE(!is.null(row.col) && is.null(getOption("knitr.in.progress")))) { row.col <- misty::chr.color(row.col, color = message.col, check = FALSE) }

  } else {

    row.col <- NULL

  }

  #_____________________________________________________________________________
  #
  # Print ----------------------------------------------------------------------

  if (isTRUE(output)) {

    # Print data frame
    write.table(object, quote = FALSE, col.names = FALSE, row.names = FALSE)

    # Print message
    if (isTRUE(!is.null(row.col))) { cat(row.col) }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(invisible(list(df = object, row.col = row.col)))

}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#' @rdname df.tail
df.tail <- function(x, n = 6, digits = 3, width = 20, row.names = TRUE, row.names.col = "gray2",
                    message = TRUE, message.col = "b.blue", check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing
  if (isTRUE(missing(x))) { stop("Please specify a data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'data' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("row.names", "message"), numeric = list(n = 1L, width = 1L), args = "digits", envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'n'
    if (isTRUE(n <= 0)) { stop("Please specify a numeric value greater than zero for the argument 'n'", call. = FALSE) }

    # Check input 'width'
    if (isTRUE(width <= 0)) { stop("Please specify a numeric value greater than zero for the argument 'width'", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Format Data Frame ####

  object <- sapply(x, function(y) {

    # Numeric variables
    if (isTRUE(is.numeric(y) && any(y %% 1L != 0L))) {

      # Number of digits > 'digits'
      if (isTRUE(any((nchar(as.character(y)) - nchar(as.integer(y))) |> (\(y) ifelse(y != 0L, y - 1L, y))() > digits))) {

        ifelse(!is.na(y), formatC(y, digits = digits, format = "f", zero.print = ifelse(digits > 0L, paste0("0.", paste(rep(0, times = digits), collapse = "")), "0")), NA)

      # Number of digits <= 'digits'
      } else {

        (max(nchar(as.character(y)) - nchar(as.integer(y))) - 1L) |> (\(w) ifelse(!is.na(y), formatC(y, digits = w, format = "f", zero.print = ifelse(w > 0L, paste0("0.", paste(rep(0, times = w), collapse = "")), "0")), NA))()

      }

    # Characters or factors
    } else if (isTRUE(is.character(y) || is.factor(y) && any(nchar(as.character(y)) > width))) {

      ifelse(!is.na(y), misty::chr.trunc(y, width = width, check = FALSE), y)

    } else {

      return(y)

    }

  }) |> (\(y) if (isTRUE(is.null(dim(y)))) { as.data.frame(t(y)) } else if (isTRUE(!is.data.frame(y))) { as.data.frame(y) } else { return(y) })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add Row Names ####

  if (isTRUE(row.names)) {

    object <- apply(rbind(names(object), object), 2L, format, justify = "right") |> (\(y) cbind(format(c("", row.names(x)), justify = "left"), y))()

  } else {

    object <- apply(rbind(names(object), object), 2L, format, justify = "right")

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Remove Rows ####

  if (isTRUE(n < nrow(x))) {

    object <- object[c(1L, (nrow(object) - n + 1L):nrow(object)), , drop = FALSE]

    if (isTRUE(row.names)) { object[, 1L] <- format(misty::chr.trim(object[, 1L]), justify = "left") }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Fit Console ####

  if (isTRUE(max(nchar(apply(object, 1L, function(y) paste(y, collapse = " ")))) > getOption("width"))) {

    object <- object[, seq_len(rev(which((cumsum(apply(object, 2L, function(y) max(nchar(y))) + 1L) - 0L) <= getOption("width")))[1L])]

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Format Row Names ####

  if (isTRUE(row.names && is.null(getOption("knitr.in.progress")))) { object[, 1L] <- misty::chr.color(object[, 1L], color = row.names.col, check = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Message ####

  if (message) {

    # Rows
    if (isTRUE(n < nrow(x))) { row.col <- (nrow(x) - n) |> (\(y) paste0(y, " more ", ifelse(y == 1L, "row", "rows")))() } else { row.col <- NULL }

    # Columns
    if (isTRUE(ncol(object) < ifelse(row.names, ncol(x) + 1L, ncol(x)))) {

      if (isTRUE(!is.null(row.col))) {

        row.col <- paste0(row.col, " and ", (ifelse(row.names, ncol(x) + 1L, ncol(x)) - ncol(object)) |> (\(y) paste0(y, " more ", ifelse(y == 1L, "variable", "variables")))())

      } else {

        row.col <- (ifelse(row.names, ncol(x) + 1L, ncol(x)) - ncol(object)) |> (\(y) paste0(y, " more ", ifelse(y == 1L, "variable", "variables")))()

      }

    }

    # Format message
    if (isTRUE(!is.null(row.col) && is.null(getOption("knitr.in.progress")))) { row.col <- misty::chr.color(row.col, color = message.col, check = FALSE) }

  } else {

    row.col <- NULL

  }

  #_____________________________________________________________________________
  #
  # Print ----------------------------------------------------------------------

  if (isTRUE(output)) {

    # Print data frame
    write.table(object, quote = FALSE, col.names = FALSE, row.names = FALSE)

    # Print message
    if (isTRUE(!is.null(row.col))) { cat(row.col) }

  }

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  return(invisible(list(df = object, row.col = row.col)))

}

#_______________________________________________________________________________
