#' Recode Variable
#'
#' This function recodes numeric vectors, character vectors, or factors according
#' to recode specifications.
#'
#' @param data      a numeric vector, character vector, factor, or data frame.
#' @param ...       an expression indicating the variable names in \code{data},
#'                  e.g., \code{rec(dat, x1, x2, x3, spec = "1 = 0"))}. Note that
#'                  the operators \code{+}, \code{-}, \code{~}, \code{:},
#'                  \code{::}, and \code{!} can also be used to select variables,
#'                  see 'Details' in the \code{\link{df.subset}} function.
#' @param spec      a character string of recode specifications (see 'Details').
#' @param as.factor logical: if \code{TRUE}, character vector will be coerced to
#'                  a factor.
#' @param levels    a character vector for specifying the levels in the returned
#'                  factor.
#' @param append    logical: if \code{TRUE} (default), centered variable(s) are
#'                  appended to the data frame specified in the argument \code{data}.
#' @param name      a character string or character vector indicating the names
#'                  of the recoded variables. By default, variables are named with
#'                  the ending \code{".r"} resulting in e.g. \code{"x1.r"} and
#'                  \code{"x2.r"}. Variable names can also be specified using a
#'                  character vector matching the number of variables specified
#'                  in \code{data} (e.g., \code{name = c("recode.x1", "recode.x2")}).
#' @param as.na     a numeric vector indicating user-defined missing values,
#'                  i.e. these values are converted to \code{NA} before conducting
#'                  the analysis.
#' @param table     logical: if \code{TRUE}, a cross table variable x recoded
#'                  variable is printed on the console if only one variable is
#'                  specified in \code{data}.
#' @param check     logical: if \code{TRUE} (default), argument specification is
#'                  checked.
#'
#' @details
#' Recode specifications appear in a character string, separated by semicolons
#' (see the examples below), of the form input = output. If an input value satisfies
#' more than one specification, then the first (from left to right) applies. If
#' no specification is satisfied, then the input value is carried over to the
#' result. \code{NA} is allowed in input and output. Several recode specifications
#' are supported:
#' \describe{
#' \item{\strong{Single Value}}{For example, \code{spec = "0 = NA".}}
#' \item{\strong{Vector of Values}}{For example, \code{spec = "c(7, 8, 9) = 'high'"}.}
#' \item{\strong{Range of Values}}{For example, \code{spec = "7:9 = 'C'"}. The
#' special values \code{lo} (lowest value) and \code{hi} (highest value) may
#' appear in a range. For example, \code{spec = "lo:10 = 1"}. Note that \code{:}
#' is not the R sequence operator. In addition you may not use \code{:} with the
#' collect operator, e.g., \code{spec = "c(1, 3, 5:7)"} will cause an error.}
#' \item{\strong{else}}{For example, \code{spec = "0 = 1; else = NA"}. Everything
#' that does not fit a previous specification. Note that \code{else} matches all
#' otherwise unspecified values on input, including \code{NA}.}
#' }
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{coding}}, \code{\link{item.reverse}}
#'
#' @references
#' Fox, J., & Weisberg S. (2019). \emph{An {R} Companion to Applied Regression} (3rd ed.).
#' Thousand Oaks CA: Sage. URL: https://socialsciences.mcmaster.ca/jfox/Books/Companion/
#'
#' @return
#' Returns a numeric vector or data frame with the same length or same number of
#' rows as \code{data} containing the recoded coded variable(s).
#'
#' @note
#' This function was adapted from the \code{recode()} function in the \pkg{car}
#' package by John Fox and Sanford Weisberg (2019).
#'
#' @export
#'
#' @examples
#' #----------------------------------------------------------------------------
#' # Example 1: Numeric vector
#' x.num <- c(1, 2, 4, 5, 6, 8, 12, 15, 19, 20)
#'
#' # Example 1a: Recode 5 = 50 and 19 = 190
#' rec(x.num, spec = "5 = 50; 19 = 190")
#'
#' # Example 1b: Recode 1, 2, and 5 = 100 and 4, 6, and 7 = 200 and else = 300
#' rec(x.num, spec = "c(1, 2, 5) = 100; c(4, 6, 7) = 200; else = 300")
#'
#' # Example 1c: Recode lowest value to 10 = 100 and 11 to highest value = 200
#' rec(x.num, spec = "lo:10 = 100; 11:hi = 200")
#'
#' # Example 1d: Recode 5 = 50 and 19 = 190 and check recoding
#' rec(x.num, spec = "5 = 50; 19 = 190", table = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Example 2: Character vector
#' x.chr <- c("a", "c", "f", "j", "k")
#'
#' # Example 2a: Recode a to x
#' rec(x.chr, spec = "'a' = 'X'")
#'
#' # Example 2b: Recode a and f to x, c and j to y, and else to z
#' rec(x.chr, spec = "c('a', 'f') = 'x'; c('c', 'j') = 'y'; else = 'z'")
#'
#' # Example 2c: Recode a to x and coerce to a factor
#' rec(x.chr, spec = "'a' = 'X'", as.factor = TRUE)
#'
#' #----------------------------------------------------------------------------
#' # Example 3: Factor
#' x.fac <- factor(c("a", "b", "a", "c", "d", "d", "b", "b", "a"))
#'
#' # Example 3a: Recode a to x, factor levels ordered alphabetically
#' rec(x.fac, spec = "'a' = 'x'")
#'
#' # Example 3b: Recode a to x, user-defined factor levels
#' rec(x.fac, spec = "'a' = 'x'", levels = c("x", "b", "c", "d"))
#'
#' #----------------------------------------------------------------------------
#' # Example 4: Multiple variables
#' dat <- data.frame(x1.num = c(1, 2, 4, 5, 6),
#'                   x2.num = c(5, 19, 2, 6, 3),
#'                   x1.chr = c("a", "c", "f", "j", "k"),
#'                   x2.chr = c("b", "c", "a", "d", "k"),
#'                   x1.fac = factor(c("a", "b", "a", "c", "d")),
#'                   x2.fac = factor(c("b", "a", "d", "c", "e")))
#'
#' # Example 4a: Recode numeric vector and attach to 'dat'
#' cbind(dat, rec(dat[, c("x1.num", "x2.num")], spec = "5 = 50; 19 = 190"))
#'
#' # Alternative specification using the 'data' argument,
#' rec(dat, x1.num, x2.num, spec = "5 = 50; 19 = 190")
#'
#' # Example 4b: Recode character vector and attach to 'dat'
#' cbind(dat, rec(dat[, c("x1.chr", "x2.chr")], spec = "'a' = 'X'"))
#'
#' # Example 4c: Recode factor vector and attach to 'dat'
#' cbind(dat, rec(dat[, c("x1.fac", "x2.fac")], spec = "'a' = 'X'"))
rec <- function(data, ..., spec, as.factor = FALSE, levels = NULL,
                append = TRUE, name = ".e", as.na = NULL, table = FALSE,
                check = TRUE) {


  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'data' is missing or NULL
  if (isTRUE(missing(data) || is.null(data))) { stop("Please specify a a numeric vector, character vector, factor, or data frame for the argument 'data'", call. = FALSE) }

  # Check if input 'spec' is missing
  if (isTRUE(missing(spec) || is.null(spec))) { stop("Please specify a character string for the argument 'spec'.", call. = FALSE) }

  #_____________________________________________________________________________
  #
  # Data -----------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data using the argument '...' ####

  if (isTRUE(!missing(...))) {

    # Variable names
    var.names <- .var.names(data = data, ...)

    # Extract data and convert tibble into data frame or vector
    x <- data[, var.names] |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Data without using the argument '...' ####

  } else {

    # Convert 'data' as tibble into data frame
    x <- data |> (\(y) if (isTRUE("tbl" %in% substr(class(y), 1L, 3L))) { if (isTRUE(ncol(as.data.frame(y)) == 1L)) { unname(unlist(y)) } else { as.data.frame(y) } } else { y })()

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) { x <- .as.na(x, na = as.na) }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check inputs
  .check.input(logical = c("as.factor", "table"), envir = environment(), input.check = check)

  # Additional checks
  if (isTRUE(check)) {

    # Check input 'name'
    if (isTRUE(!is.null(dim(x)))) {

      if (isTRUE(!is.character(name))) { stop("Please specify a character string or vector for the argument 'name'.", call. = FALSE) }

      if (isTRUE(length(name) > 1L && length(name) != ncol(x))) { stop("The length of the vector specified in 'name' does not match with the number of variable in 'data'.", call. = FALSE) }

    }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Define special values ####

  lo <- -Inf
  hi <- Inf

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Recode specification terms ####

  spec.list <- rev(unlist(strsplit(spec, ";")))

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Single variable ####
  if (isTRUE(is.null(dim(x)))) {

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Recoded result vector ####

    object <- x

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Convert factor into character ####

    if (isTRUE(is.factor(object))) { object <- as.character(object) }

    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## Loop across specification terms ####

    for (i in spec.list) {

      #...................
      ### Specification with range of values ####

      if (isTRUE(length(grep(":", i)) == 1L)) {

        range <- unlist(strsplit(unlist(strsplit(i, "="))[1L], ":"))

        low <- try(eval(parse(text = range[1L])), silent = TRUE)

        if (isTRUE(class(low) == "try-error")) {

          stop("In recode specification term: ", i, "\n       Message: ", attributes(low)$condition$message, call. = FALSE)

        }

        high <- try(eval(parse(text = range[2L])), silent = TRUE)

        if (isTRUE(class(high) == "try-error")) {

          stop("In recode specification term: ", i, "\n       Message: ", attributes(high)$condition$message, call. = FALSE)

        }

        target <- try(eval(parse(text = unlist(strsplit(i, "="))[2L])), silent = TRUE)

        if (isTRUE(class(target) == "try-error")) {

          stop("In recode specification term: ", i, "\n       Message: ", attributes(target)$condition$message, call. = FALSE)

        }

        object[(x >= low) & (x <= high)] <- target

      }

      #...................
      ### Specification with range of values else ####

      if (isTRUE(length(grep("else", i)) == 1L)) {

        target <- try(eval(parse(text = unlist(strsplit(i, "="))[2L])), silent = TRUE)

        if (isTRUE(class(target) == "try-error")) {

          stop("In recode specification term: ", i, "\n       Message: ", attributes(target)$condition$message, call. = FALSE)

        }

        object[seq_len(length(x))] <- target

      }

      #...................
      ### Specification with single or vector of values ####

      if (isTRUE(length(grep(":", i))  == 0L && length(grep("else", i)) == 0L)) {

        set <- try(eval(parse(text = unlist(strsplit(i, "="))[1L])), silent = TRUE)

        if (isTRUE(class(set) == "try-error")) {

          stop("In recode specification term: ", i, "\n       Message: ", attributes(set)$condition$message, call. = FALSE)

        }

        target <- try(eval(parse(text = unlist(strsplit(i, "="))[2L])), silent = TRUE)

        if (isTRUE(class(target) == "try-error")) {

          stop("In recode specification term: ", i, "\n       Message: ", attributes(target)$condition$message, call. = FALSE)

        }

        for (j in set) {

          if (isTRUE(is.na(j)))  {

            object[is.na(x)] <- target

          } else {

            object[x == j] <- target

          }

        }

      }

    }

    #...................
    ### Character and factor ####

    if (isTRUE(is.character(object))) {

      #......
      # Original vector was a factor
      if (isTRUE(is.factor(x))) {

        if (isTRUE(is.null(levels))) {

          object <- factor(object, levels = c(intersect(levels(x), object), setdiff(object, levels(x))))

        } else {

          object <- factor(object, levels = levels)

        }

      #......
      # Original vector was not a factor
      } else {

        if (isTRUE(as.factor)) {

          if (isTRUE(is.null(levels))) {

            object <- factor(object)

          } else {

            object <- factor(object, levels = levels)

          }

        #......
        # Convert character in numeric if possible
        } else {

          object.test <- suppressWarnings(as.numeric(object))

          if (isTRUE(sum(is.na(object.test)) == sum(is.na(object)))) {

            object <- as.numeric(object)

          }

        }

      }

    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Multiple variables ####
  } else {

    object <- data.frame(lapply(x, misty::rec, spec = spec, as.factor = as.factor, levels = levels, as.na = as.na, check = FALSE))

    #...................
    ### Variable names ####

    if (isTRUE(length(name) == 1L)) {

      colnames(object) <- paste0(colnames(object), name)

    } else {

      colnames(object) <- name

    }

  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print cross table ####

  if (isTRUE(is.null(dim(x)) && table)) { print(table(x, object, dnn = c("item", "recoded coded"), useNA = "always")) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Append ####

  if (isTRUE(!missing(...) && append)) {

    if (isTRUE(is.null(dim(x)))) {

      #...................
      ### Variable names ####

      if (isTRUE(name == ".e")) {

        object <- setNames(as.data.frame(object), nm = paste0(var.names, ".e"))

      } else {

        object <- setNames(as.data.frame(object), nm = name)

      }

    }

    object <- data.frame(data, object)

  }

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(is.null(dim(x)) && table)) { return(invisible(object)) } else { return(object) }

}

#_______________________________________________________________________________
