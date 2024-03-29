#' Combine Data Frames by Rows, Filling in Missing Columns
#'
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
#' @param ...      a sequence of data frame to be row bind together. This argument
#'                 can be a list of data frames, in which case all other arguments
#'                 are ignored. Any \code{NULL} inputs are silently dropped. If
#'                 all inputs are \code{NULL}, the output is also \code{NULL}.
#'
#' @author
#' Hadley Wickham
#'
#' @seealso
#' \code{\link{df.duplicated}}, \code{\link{df.merge}},  \code{\link{df.move}},
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
#' adat <- data.frame(id = c(1, 2, 3),
#'                    a = c(7, 3, 8),
#'                    b = c(4, 2, 7))
#'
#' bdat <- data.frame(id = c(4, 5, 6),
#'                    a = c(2, 4, 6),
#'                    c = c(4, 2, 7))
#'
#' cdat <- data.frame(id = c(7, 8, 9),
#'                    a = c(1, 4, 6),
#'                    d = c(9, 5, 4))
#'
#' # Example 1
#' df.rbind(adat, bdat, cdat)
df.rbind <- function(...) {

  #_____________________________________________________________________________
  #
  # Functions ---------------------------------------------------------------

  make_names <- function(x, prefix = "X") {

    nm <- names(x)

    if (isTRUE(is.null(nm))) {

      nm <- rep.int("", length(x))

    }

    n <- sum(nm == "", na.rm = TRUE)

    nm[nm == ""] <- paste0(prefix, seq_len(n))

    return(nm)
  }

  quickdf <- function (list) {

    rows <- unique(unlist(lapply(list, NROW)))

    stopifnot(length(rows) == 1L)

    names(list) <- make_names(list, "X")

    class(list) <- "data.frame"

    attr(list, "row.names") <- c(NA_integer_, -rows)

    return(list)

  }

  make_assignment_call <- function (ndims) {

    assignment <- quote(column[rows] <<- what)

    if (isTRUE(ndims >= 2L)) {

      assignment[[2L]] <- as.call(c(as.list(assignment[[2]]), rep(list(quote(expr = )), ndims - 1L)))

    }

    return(assignment)

  }

  allocate_column <- function(example, nrows, dfs, var) {

    a <- attributes(example)
    type <- typeof(example)
    class <- a$class
    isList <- is.recursive(example)

    a$names <- NULL
    a$class <- NULL

    if (isTRUE(is.data.frame(example))) {

      stop("Data frame column '", var, "' not supported by the df.rbind() function.", call. = FALSE)

    }

    if (isTRUE(is.array(example))) {

      if (isTRUE(length(dim(example)) > 1L)) {

        if (isTRUE("dimnames" %in% names(a))) {

          a$dimnames[1L] <- list(NULL)

          if (isTRUE(!is.null(names(a$dimnames))))

            names(a$dimnames)[1L] <- ""

        }

        # Check that all other args have consistent dims
        df_has <- vapply(dfs, function(df) var %in% names(df), FALSE)

        dims <- unique(lapply(dfs[df_has], function(df) dim(df[[var]])[-1]))

        if (isTRUE(length(dims) > 1L))

          stop("Array variable ", var, " has inconsistent dimensions.", call. = FALSE)

        a$dim <- c(nrows, dim(example)[-1L])

        length <- prod(a$dim)

      } else {

        a$dim <- NULL
        a$dimnames <- NULL
        length <- nrows

      }

    } else {

      length <- nrows

    }

    if (isTRUE(is.factor(example))) {

      df_has <- vapply(dfs, function(df) var %in% names(df), FALSE)

      isfactor <- vapply(dfs[df_has], function(df) is.factor(df[[var]]), FALSE)

      if (isTRUE(all(isfactor))) {

        levels <- unique(unlist(lapply(dfs[df_has], function(df) levels(df[[var]]))))

        a$levels <- levels

        handler <- "factor"

      } else {

        type <- "character"
        handler <- "character"
        class <- NULL
        a$levels <- NULL

      }

    } else if (isTRUE(inherits(example, "POSIXt"))) {

      tzone <- attr(example, "tzone")
      class <- c("POSIXct", "POSIXt")
      type <- "double"
      handler <- "time"

    } else {

      handler <- type

    }

    column <- vector(type, length)

    if (isTRUE(!isList)) {

      column[] <- NA

    }

    attributes(column) <- a

    assignment <- make_assignment_call(length(a$dim))

    setter <- switch(
      handler,
      character = function(rows, what) {
        what <- as.character(what)
        eval(assignment)
      },
      factor = function(rows, what) {
        #duplicate what `[<-.factor` does
        what <- match(what, levels)
        #no need to check since we already computed levels
        eval(assignment)
      },
      time = function(rows, what) {
        what <- as.POSIXct(what, tz = tzone)
        eval(assignment)
      },
      function(rows, what) {
        eval(assignment)
      })

    getter <- function() {
      class(column) <<- class

      column

    }

    list(set = setter, get = getter)

  }

  output_template <- function(dfs, nrows) {

    vars <- unique(unlist(lapply(dfs, base::names)))
    output <- vector("list", length(vars))
    names(output) <- vars

    seen <- rep(FALSE, length(output))
    names(seen) <- vars

    for (df in dfs) {

      matching <- intersect(names(df), vars[!seen])

      for (var in matching) {

        output[[var]] <- allocate_column(df[[var]], nrows, dfs, var)

      }

      seen[matching] <- TRUE
      if (isTRUE(all(seen))) break

    }

    list(setters = lapply(output, `[[`, "set"), getters = lapply(output, `[[`, "get"))
  }

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  dfs <- list(...)

  if (isTRUE(length(dfs) == 0L)) {

    return()

  }

  if (isTRUE(is.list(dfs[[1L]]) && !is.data.frame(dfs[[1]]))) {

    dfs <- dfs[[1L]]

  }

  dfs <- Filter(Negate(is.null), dfs)

  if (isTRUE(length(dfs) == 0L)) {

    return()

  }

  if (isTRUE(length(dfs) == 1L)) {

    return(dfs[[1L]])

  }

  is_df <- vapply(dfs, is.data.frame, FUN.VALUE = logical(1))

  if (isTRUE(any(!is_df))) {

    stop("Please specify data frames for all inputs of the df.rbind() function.", call. = FALSE)

  }

  rows <- vapply(dfs, .row_names_info, type = 2L, FUN.VALUE = 1L)

  nrows <- sum(rows)

  ot <- output_template(dfs, nrows)

  setters <- ot$setters
  getters <- ot$getters

  if (isTRUE(length(setters) == 0L)) {

    return(as.data.frame(matrix(nrow = nrows, ncol = 0L), stringsAsFactors = FALSE))

  }

  pos <- matrix(c(cumsum(rows) - rows + 1L, rows), ncol = 2L)

  for (i in seq_along(rows)) {

    rng <- seq(pos[i, 1], length.out = pos[i, 2L])

    df <- dfs[[i]]

    for (var in names(df)) {

      setters[[var]](rng, df[[var]])

    }

  }

  quickdf(lapply(getters, function(x) x()))

}
