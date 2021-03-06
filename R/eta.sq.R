#' Eta Squared
#'
#' This function computes eta squared for one or more outcome variables in combination with one or more
#' grouping variables.
#'
#' @param x           a numeric vector, matrix or data frame with numeric vectors for the outcome variables.
#' @param group       a vector, matrix or data frame with integer vectors, character vectors or factors
#'                    for the grouping variables.
#' @param digits      an integer value indicating the number of decimal places  to be used for displaying
#'                    eta squared.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#'                    Note that \code{as.na()} function is only applied to the argument \code{x}.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cohens.d}}, \code{\link{cor.cont}}, \code{\link{cor.matrix}},
#' \code{\link{cor.cramer}}, \code{\link{cor.phi}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#' function call (\code{call}), matrix or data frame specified in \code{x} (\code{data}), specification of
#' function arguments (\code{args}), and list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(1, 1, 1, 1, 2, 2, 2, 2, 2),
#'                   x2 = c(1, 1, 1, 2, 2, 2, 3, 3, 3),
#'                   y1 = c(3, 2, 4, 5, 6, 4, 7, 5, 7),
#'                   y2 = c(2, 4, 1, 5, 3, 3, 4, 6, 7))
#'
#' # Eta squared for y1 explained by x1
#' eta.sq(dat$y1, group = dat$x1)
#'
#' # Eta squared for y1 and y2 explained by x1 and x2
#' eta.sq(dat[, c("y1", "y2")], group = dat[, c("x1", "x2")])
eta.sq <- function(x, group, digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) {

    stop("Please specify a vector, matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) {

    stop("Input specified for the argument 'x' is NULL.", call. = FALSE)

  }

  #......
  # Vector, matrix or data frame for the argument 'x'?
  if (isTRUE(!is.atomic(x) && !is.matrix(x) && !is.data.frame(x))) {

    stop("Please specify a vector, matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check input 'group'
  if (isTRUE(missing(group))) {

    stop("Please specify a vector, matrix or data frame for the argument 'group'.", call. = FALSE)

  }

  #----------------------------------------
  # Data frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }


  #.........................................

  if (isTRUE(check)) {

    #......
    # Check input 'x': Is numeric?
    if (isTRUE(any(vapply(x, function(y) !is.numeric(y), FUN.VALUE = logical(1))))) {

      stop("Please specify a numeric vector, matrix or data frame with numeric vectors for the argument 'x'.",
           call. = FALSE)

    }

    #......
    # Check input 'x': Zero variance?
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1))

    if (isTRUE(any(x.zero.var))) {

      if (isTRUE(length(x.zero.var) > 1L)) {

        warning(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ",
                       paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

      } else {

        stop("Vector specified in 'x' has zero variance.", call. = FALSE)

      }

    }

    #......
    # Check input 'group': Is integer, character or factors?
    if (isTRUE(any(vapply(as.data.frame(group, stringsAsFactors = FALSE), function(y) any(as.numeric(y) %% 1L != 0L, na.rm = TRUE), FUN.VALUE = logical(1L))))) {

      stop("Please specify a integer vector, matrix or data frame with integer vectors, character vectors or factors for the argument 'x'",
           call. = FALSE)

    }

    #......
    # Check input 'group': At least two groups?
    group.check <- vapply(as.data.frame(group, stringsAsFactors = FALSE), function(y) length(na.omit(unique(y))) < 2L, FUN.VALUE = logical(1))

    if (isTRUE(any(group.check))) {

      if (isTRUE(length(group.check) > 1L)) {

        stop(paste0("Follwing grouping variables specified in 'group' do not have at least two groups: ",
                    paste(names(which(group.check)), collapse = ", ")), call. = FALSE)

      } else {

        stop("Grouping variable specified in 'group' does not have at least two groups.", call. = FALSE)

      }

    }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

      warning("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    # Constant variables
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))
    if (isTRUE(any(x.zero.var))) {

      stop(paste0("After converting user-missing values into NA, following variables have only one unique value: ",
                  paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

  }

  #-----------------------------------------
  # Number of dependent variables, number of independent variables

  x.ncol <- ncol(x) == 1L

  group.dim.null <- is.null(dim(group))

  ####################################################################################
  # Main Function

  #----------------------------------------
  # One dependent variable, one independent variable

  if (isTRUE(x.ncol && group.dim.null)) {

      # Estimate model
      mod <- aov(x ~ factor(group), data = data.frame(x, group, stringsAsFactors = FALSE))

      # Model summary
      mod.summary <- summary(mod)

      # Mean Squared Error Between
      SQ.B <- unlist(mod.summary[[1L]])["Sum Sq1"]

      # Mean Squared Error Within
      SQ.T <- SQ.B + unlist(mod.summary[[1L]])["Sum Sq2"]

      # Eta squared
      eta <- unname(SQ.B / SQ.T)

      # NaN to NA
      eta <- ifelse(is.nan(eta), NA, eta)

  }

  #----------------------------------------
  # More than one dependent variable, one independent variable

  if (isTRUE(!x.ncol && group.dim.null)) {

    eta <- matrix(NA, ncol = ncol(x), dimnames = list(NULL, colnames(x)))
    for (i in seq_len(ncol(x))) {

      eta[, i] <- misty::eta.sq(x[, i], group = group, check = FALSE, output = FALSE)$result

    }

  }

  #----------------------------------------
  # One dependent variable, more than one independent variable

  if (isTRUE(x.ncol && !group.dim.null)) {

    eta <-  matrix(NA, nrow = ncol(group), dimnames = list(colnames(group), NULL))
    for (i in seq_len(ncol(group))) {

      eta[i, ] <- misty::eta.sq(x, group[, i], check = FALSE, output = FALSE)$result

    }

  }

  #----------------------------------------
  # More than one dependent variable, more than one independent variable

  if (isTRUE(!x.ncol && !group.dim.null)) {

    eta <- matrix(NA, ncol = ncol(x), nrow = ncol(group), dimnames = list(colnames(group), colnames(x)))

    # Each group
    for (i in seq_len(ncol(x))) {

      # For each dependent variable
      for (j in seq_len(ncol(group))) {

        eta[j, i] <- misty::eta.sq(x[, i], group[, j], check = FALSE, output = FALSE)$result

      }

    }

  }

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "eta.sq",
                 dat = list(x = x, group = group),
                 args = list(digits = digits, as.na = as.na, check = check, output = output),
                 result = eta)

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
