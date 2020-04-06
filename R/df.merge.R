#' Merge Multiple Data Frames
#'
#' This function merges data frames by a common column (i.e., matching variable).
#'
#' There are following requirements for merging multiple data frames: First, each data frame
#' has the same matching variable specified in the \code{by} argument. Second, matching variable
#' in the data frames have all the same class. Third, there are no duplicated values in the
#' matching variable in each data frame. Fourth, there are no missing values in the matching
#' variables. Last, there are no duplicated variable names across the data frames except for
#' the matching variable.
#'
#' Note that it is possible to specify data frames matrices and/or in the argument \code{...}.
#' However, the function always returns a data frame.
#'
#' @param ...       a sequence of matrices or data frames and/or matrices to be merged to one.
#' @param by        a character string indicating the column used for merging (i.e., matching variable),
#'                  see 'Details'.
#' @param all       logical: if \code{TRUE}, then extra rows with \code{NA}s will be added
#'                  to the output for each row in a data frame that has no matching row in
#'                  another data frame.
#' @param check     logical: if \code{TRUE}, argument specification is checked.
#' @param output    logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{df.rbind}}, \code{\link{df.rename}}, \code{\link{df.sort}}
#'
#' @return
#' Returns a merged data frame.
#'
#' @export
#'
#' @examples
#' adat <- data.frame(id = c(1, 2, 3),
#'                    x1 = c(7, 3, 8), stringsAsFactors = FALSE)
#'
#' bdat <- data.frame(id = c(1, 2),
#'                    x2 = c(5, 1), stringsAsFactors = FALSE)
#'
#' cdat <- data.frame(id = c(2, 3),
#'                    y3 = c(7, 9), stringsAsFactors = FALSE)
#'
#' ddat <- data.frame(id = 4,
#'                    y4 = 6, stringsAsFactors = FALSE)
#'
#' # Merge adat, bdat, cdat, and data by the variable id
#' df.merge(adat, bdat, cdat, ddat, by = "id")
#'
#' # Do not show output on the console
#' df.merge(adat, bdat, cdat, ddat, by = "id", output = FALSE)
#'
#' \dontrun{
#' #--------------------------------------#'
#' # Error messages
#'
#' adat <- data.frame(id = c(1, 2, 3),
#'                    x1 = c(7, 3, 8), stringsAsFactors = FALSE)
#'
#' bdat <- data.frame(code = c(1, 2, 3),
#'                    x2 = c(5, 1, 3), stringsAsFactors = FALSE)
#'
#' cdat <- data.frame(id = factor(c(1, 2, 3)),
#'                    x3 = c(5, 1, 3), stringsAsFactors = FALSE)
#'
#' ddat <- data.frame(id = c(1, 2, 2),
#'                    x2 = c(5, 1, 3), stringsAsFactors = FALSE)
#'
#' edat <- data.frame(id = c(1, NA, 3),
#'                    x2 = c(5, 1, 3), stringsAsFactors = FALSE)
#'
#' fdat <- data.frame(id = c(1, 2, 3),
#'                    x1 = c(5, 1, 3), stringsAsFactors = FALSE)
#'
#' # Error: Data frames do not have the same matching variable specified in 'by'.
#' df.merge(adat, bdat, by = "id")
#'
#' # Error: Matching variable in the data frames do not all have the same class.
#' df.merge(adat, cdat, by = "id")
#'
#' # Error: There are duplicated values in the matching variable specified in 'by'.
#' df.merge(adat, ddat, by = "id")
#'
#' # Error: There are missing values in the matching variable specified in 'by'.
#' df.merge(adat, edat, by = "id")
#'
#' #' # Error: There are duplicated variable names across data frames.
#' df.merge(adat, fdat, by = "id")
#' }
df.merge <- function(..., by, all = TRUE, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  # List of data frames
  df <- lapply(list(...), as.data.frame, stringsAsFactors = FALSE)

  ####################################################################################
  # Input Check

  #......
  # Check input 'by'
  if (missing(by)) {

    stop("Please specify a character string for the argument 'by'.", call. = FALSE)

  }

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #----------------------------------------

  if (isTRUE(check)) {

    #......
    # Same matching variable in each data frame
    if (any(vapply(df, function(y) !by %in%  names(y), FUN.VALUE = logical(1L)))) {

      stop("Data frames do not have the same matching variable specified in 'by'.", call. = FALSE)

    }

    #......
    # Same class
    if (length(unique(vapply(df, function(y) class(y[, by]), FUN.VALUE = character(1L)))) != 1L) {

      stop("Matching variable in the data frames do not all have the same class.", call. = FALSE)

    }

    #......
    # Duplicated values in the matching variable
    if (any(vapply(df, function(y) anyDuplicated(na.omit(y[, by])), FUN.VALUE = 1L) != 0L)) {

      stop("There are duplicated values in the matching variable specified in 'by'.", call. = FALSE)

    }

    #......
    # Missing values in the matching variable
    if (any(vapply(df, function(y) any(is.na(y[, by])), FUN.VALUE = logical(1L)))) {

      stop("There are missing values in the matching variable specified in 'by'.", call. = FALSE)

    }

    #......
    # Duplicated variable names across the data frames
    if (anyDuplicated(unlist(lapply(df, names))[unlist(lapply(df, names)) != by]) != 0L) {

      stop("There are duplicated variable names across data frames.", call. = FALSE)

    }

    #......
    # Check input 'all'
    if (isFALSE(isTRUE(all) | isFALSE(all))) {

      stop("Please specify TRUE or FALSE for the argument 'all'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) || isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Main Function

  # Number of data frames
  no.dfs <- length(df)

  # Name of data frames
  df.names <- as.character(match.call())[2L:(no.dfs + 1L)]

  # Number of variables in each data frame
  no.var <- vapply(df, ncol, FUN.VALUE = 1L)

  # Number of cases in each data frame
  no.cases <- vapply(df, nrow, FUN.VALUE = 1L)

  # Matching variable
  var.match <- lapply(df, function(y) y[, by])

  # Match data frames
  match.cases <- Reduce(function(xx, yy) misty::df.rbind(xx, yy), x = lapply(var.match, function(xx) data.frame(matrix(xx, ncol = length(xx), dimnames = list(NULL, xx)), stringsAsFactors = FALSE)))

  # Number of pattern
  match.cases.table <- table(apply(ifelse(is.na(match.cases), 0L, 1L), 2, paste, collapse = " "))

  match.cases.table <- match.cases.table[rev(order(as.numeric(gsub(" ", "", names(match.cases.table)))))]

  match.info <- data.frame(n = unname(unclass(match.cases.table)),
                           matrix(unlist(strsplit(names(match.cases.table), " ")), byrow = TRUE, ncol = no.dfs,
                           dimnames = list(NULL, df.names)),
                           stringsAsFactors = FALSE)

  # Match data frames
  object <- Reduce(function(xx, yy) merge(xx, yy, by = by, all = all), x = df)

  # Sort by matching variable
  object <- misty::df.sort(object, object[, by])

  ####################################################################################
  # Print Output

  if (isTRUE(output)) {

    print.object <- rbind(c("No. of data frames", no.dfs, rep("", times = no.dfs - 1L)),
                          rep("", times = no.dfs + 1L),
                          cbind(c("", "No. of variables", "No. of cases"), rbind(df.names, no.var, no.cases)))

    print.object[, 1L] <- paste(" ", print.object[, 1L])

    # Format
    print.object[, 1L] <- format(print.object[, 1L], justify = "left")

    print.object[, -1L] <- format(print.object[, -1], justify = "right")

    match.info <- format(data.frame(rbind(c("n", df.names), match.info), stringsAsFactors = FALSE), justify = "right")

    match.info[, 1L] <- paste("  ", match.info[, 1L])

    #........................................
    # Print output

    cat(" Merge Multiple Data Frames\n\n")

    write.table(print.object, quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat("\n  Pattern of matching cases across data frames\n")

    write.table(match.info, quote = FALSE, row.names = FALSE, col.names = FALSE)

    cat("\n")

  }

  ####################################################################################
  # Return object

  return(object)

}
