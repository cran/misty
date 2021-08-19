#' Recode Variable
#'
#' This function recodes a numeric vector, character vector, or factor according to recode specifications.
#'
#' Recode specifications appear in a character string, separated by semicolons (see the examples below),
#' of the form input = output. If an input value satisfies more than one specification, then the first
#' (from left to right) applies. If no specification is satisfied, then the input value is carried over
#' to the result. \code{NA} is allowed in input and output. Several recode specifications are supported:
#'
#' - single value
#' For example, 0 = NA
#'
#' - vector of values
#' For example, c(7, 8, 9) = 'high'
#'
#' - range of values
#' For example, 7:9 = 'C'. The special values lo (lowest value) and hi (highest value) may appear in a range.
#' For example, \code{lo:10 = 1}. Note that \code{:} is not the R sequence operator. In addition you may not
#' use \code{:} with the collect operator, e.g., \code{c(1, 3, 5:7)} will cause an error.
#'
#' - else
#' For example, \code{else = NA}. Everything that does not fit a previous specification. Note that
#' \code{else} matches all otherwise unspecified values on input, including \code{NA}.
#'
#' Note that the function was adapted from the \code{recode()} function in the \pkg{car} package by
#' John Fox and Sanford Weisberg (2019).
#'
#' @param x           a numeric vector, character vector or factor.
#' @param spec        a character string of recode specifications (see 'Details').
#' @param as.factor   logical: if \code{TRUE}, chara1cter vector will be coerced to a factor.
#' @param levels      a character vector for specifying the levels in the returned factor.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param table       logical: if \code{TRUE}, a cross table variable x recoded variable is printed
#'                    on the console.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{item.reverse}}
#'
#' @references
#' Fox, J., & Weisberg S. (2019). \emph{An {R} Companion to Applied Regression} (3rd ed.).
#' Thousand Oaks CA: Sage. URL: https://socialsciences.mcmaster.ca/jfox/Books/Companion/
#'
#' @return
#' Returns a numeric vector with the same length as \code{x} containing the recoded variable.
#'
#' @export
#'
#' @examples
#' #--------------------------------------
#' # Numeric vector
#' x.num <- c(1, 2, 4, 5, 6, 8, 12, 15, 19, 20)
#'
#' # Recode 5 = 50 and 19 = 190
#' rec(x.num, "5 = 50; 19 = 190")
#'
#' # Recode 1, 2, and 5 = 100 and 4, 6, and 7 = 200 and else = 300
#' rec(x.num, "c(1, 2, 5) = 100; c(4, 6, 7) = 200; else = 300")
#'
#' # Recode lowest value to 10 = 100 and 11 to highest value = 200
#' rec(x.num, "lo:10 = 100; 11:hi = 200")
#'
#' # Recode 5 = 50 and 19 = 190 and check recoding
#' rec(x.num, "5 = 50; 19 = 190", table = TRUE)
#'
#' #--------------------------------------
#' # Character vector
#' x.chr <- c("a", "c", "f", "j", "k")
#'
#' # Recode a to x
#' rec(x.chr, "'a' = 'X'")
#'
#' # Recode a and f to x, c and j to y, and else to z
#' rec(x.chr, "c('a', 'f') = 'x'; c('c', 'j') = 'y'; else = 'z'")
#'
#' # Recode a to x and coerce to a factor
#' rec(x.chr, "'a' = 'X'", as.factor = TRUE)
#'
#' #--------------------------------------
#' # Factor
#' x.factor <- factor(c("a", "b", "a", "c", "d", "d", "b", "b", "a"))
#'
#' # Recode a to x, factor levels ordered alphabetically
#' rec(x.factor, "'a' = 'x'")
#'
#' # Recode a to x, user-defined factor levels
#' rec(x.factor, "'a' = 'x'", levels = c("x", "b", "c", "d"))
rec <- function(x, spec, as.factor = FALSE, levels = NULL, as.na = NULL, table = FALSE,
                check = TRUE) {

  ####################################################################################
  # Input Check

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Check if only one variable specified in the input 'x'
  if (ncol(data.frame(x)) != 1) {

    stop("More than one variable specified for the argument 'x'.",call. = FALSE)

  }

  #......
  # Convert 'x' into a vector
  x <- unlist(x, use.names = FALSE)

  #......
  # Check if input 'spec' is missing
  if (isTRUE(missing(spec))) {

    stop("Please specify a matrix or data frame for the argument 'spec'.", call. = FALSE)

  }

  #......
  # Check if input 'spec' is NULL
  if (isTRUE(is.null(spec))) {

    stop("Input specified for the argument 'spec' is NULL.", call. = FALSE)

  }

  #------------------------------------------

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  if (isTRUE(check)) {

    #......
    # Check input 'as.factor'
    if (isTRUE(!is.logical(as.factor))) {

      stop("Please specify TRUE or FALSE for the argument 'as.factor'.", call. = FALSE)

    }

    #......
    # Check input 'table'
    if (isTRUE(!is.logical(table))) {

      stop("Please specify TRUE or FALSE for the argument 'table'.", call. = FALSE)

    }

  }

  #----------------------------------------
  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) {

    df <- misty::as.na(df, na = as.na)

  }

  #----------------------------------------
  # Define special values
  lo <- -Inf
  hi <- Inf

  #----------------------------------------
  # Recoded result vector
  object <- x

  #----------------------------------------
  # Convert factor into character
  if (isTRUE(is.factor(x))) {

    object <- as.character(x)

  }

  #----------------------------------------
  # Recode specification terms

  spec.list <- rev(unlist(strsplit(spec, ";")))

  ####################################################################################
  # Main Function

  for (i in spec.list) {

    #----------------------------------------
    # Specification with range of values :

    if (isTRUE(length(grep(":", i)) == 1)) {

      range <- unlist(strsplit(unlist(strsplit(i, "="))[1], ":"))

      low <- try(eval(parse(text = range[1])), silent = TRUE)

      if (isTRUE(class(low) == "try-error")) {

        stop("In recode specification term: ", i, "\n       Message: ", attributes(low)$condition$message, call. = FALSE)

      }

      high <- try(eval(parse(text = range[2])), silent = TRUE)

      if (isTRUE(class(high) == "try-error")) {

        stop("In recode specification term: ", i, "\n       Message: ", attributes(high)$condition$message, call. = FALSE)

      }

      target <- try(eval(parse(text = unlist(strsplit(i, "="))[2])), silent = TRUE)

      if (isTRUE(class(target) == "try-error")) {

        stop("In recode specification term: ", i, "\n       Message: ", attributes(target)$condition$message, call. = FALSE)

      }

      object[(x >= low) & (x <= high)] <- target

    }

    #----------------------------------------
    # Specification with range of values else

    if (isTRUE(length(grep("else", i)) == 1)) {

      target <- try(eval(parse(text = unlist(strsplit(i, "="))[2])), silent = TRUE)

      if (isTRUE(class(target) == "try-error")) {

        stop("In recode specification term: ", i, "\n       Message: ", attributes(target)$condition$message, call. = FALSE)

      }

      object[seq_len(length(x))] <- target

    }

    #----------------------------------------
    # Specification with single or vector of values

    if (isTRUE(length(grep(":", i))  == 0 && length(grep("else", i)) == 0)) {

      set <- try(eval(parse(text = unlist(strsplit(i, "="))[1])), silent = TRUE)

      if (isTRUE(class(set) == "try-error")) {

        stop("In recode specification term: ", i, "\n       Message: ", attributes(set)$condition$message, call. = FALSE)

      }

      target <- try(eval(parse(text = unlist(strsplit(i, "="))[2])), silent = TRUE)

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

  #----------------------------------------
  # Character and factor

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

  ####################################################################################
  # Return object

  #----------------------------------------
  # Print cross table

  if (isTRUE(table)) {

    print(table(x, object, dnn = c("variable", "recoded variable")))

    return(invisible(object))

  } else {

    return(object)

  }

}
