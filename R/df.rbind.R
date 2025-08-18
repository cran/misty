#' Combine Data Frames by Rows, Filling in Missing Columns
#'
#' @param ...      a sequence of data frame to be row bind together. This argument
#'                 can be a list of data frames, in which case all other arguments
#'                 are ignored. Any \code{NULL} inputs are silently dropped. If
#'                 all inputs are \code{NULL}, the output is also \code{NULL}.
#'
#' @details
#' This function takes a sequence of data frames and combines them by rows, while
#' filling in missing columns with \code{NA}s.
#'
#' This is an enhancement to \code{\link{rbind}} that adds in columns that are
#' not present in all inputs, accepts a sequence of data frames, and operates
#' substantially faster.
#'
#' Column names and types in the output will appear in the order in which they
#' were encountered.
#'
#' Unordered factor columns will have their levels unified and character data
#' bound with factors will be converted to character. POSIXct data will be converted
#' to be in the same time zone. Array and matrix columns must have identical
#' dimensions after the row count. Aside from these there are no general checks
#' that each column is of consistent data type.
#'
#' @author
#' Hadley Wickham
#'
#' @seealso
#' \code{\link{df.check}}, \code{\link{df.duplicated}}, \code{\link{df.unique}},
#' \code{\link{df.head}}, \code{\link{df.tail}}, \code{\link{df.long}},
#' \code{\link{df.wide}}, \code{\link{df.merge}}, \code{\link{df.move}},
#' \code{\link{df.rename}}, \code{\link{df.sort}}, \code{\link{df.subset}}
#'
#' @references
#' Wickham, H. (2011). The split-apply-combine strategy for data analysis.
#' \emph{Journal of Statistical Software, 40}, 1-29. https://doi.org/10.18637/jss.v040.i01
#'
#' Wickham, H. (2019). plyr: Tools for Splitting, Applying and Combining Data.
#' R package version 1.8.5.
#'
#' @return
#' Returns a single data frame
#'
#' @note
#' This function is a copy of the \code{rbind.fill()} function in the \pkg{plyr}
#' package by Hadley Wickham.
#'
#' @export
#'
#' @examples
#' adat <- data.frame(id = c(1, 2, 3), a = c(7, 3, 8), b = c(4, 2, 7))
#'
#' bdat <- data.frame(id = c(4, 5, 6), a = c(2, 4, 6), c = c(4, 2, 7))
#'
#' cdat <- data.frame(id = c(7, 8, 9), a = c(1, 4, 6), d = c(9, 5, 4))
#'
#' # Example 1
#' df.rbind(adat, bdat, cdat)
df.rbind <- function(...) {

  #_____________________________________________________________________________
  #
  # Functions ---------------------------------------------------------------


  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  dfs <- list(...)

  if (isTRUE(length(dfs) == 0L)) { return() }

  if (isTRUE(is.list(dfs[[1L]]) && !is.data.frame(dfs[[1L]]))) { dfs <- dfs[[1L]] }

  dfs <- Filter(Negate(is.null), dfs)

  if (isTRUE(length(dfs) == 0L)) { return() }

  if (isTRUE(length(dfs) == 1L)) { return(dfs[[1L]]) }

  is_df <- vapply(dfs, is.data.frame, FUN.VALUE = logical(1))

  if (isTRUE(any(!is_df))) { stop("Please specify data frames for all inputs of the df.rbind() function.", call. = FALSE) }

  rows <- vapply(dfs, .row_names_info, type = 2L, FUN.VALUE = 1L)

  nrows <- sum(rows)

  ot <- .output_template(dfs, nrows)

  setters <- ot$setters
  getters <- ot$getters

  if (isTRUE(length(setters) == 0L)) { return(as.data.frame(matrix(nrow = nrows, ncol = 0L), stringsAsFactors = FALSE)) }

  pos <- matrix(c(cumsum(rows) - rows + 1L, rows), ncol = 2L)

  for (i in seq_along(rows)) {

    rng <- seq(pos[i, 1], length.out = pos[i, 2L])

    df <- dfs[[i]]

    for (var in names(df)) { setters[[var]](rng, df[[var]]) }

  }

  .quickdf(lapply(getters, function(x) x()))

}

#_______________________________________________________________________________
