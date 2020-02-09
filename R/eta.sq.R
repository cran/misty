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
#' \code{\link{cohens.d}}, \code{\link{cont.coef}}, \code{\link{cor.matrix}},
#' \code{\link{cramers.v}}, \code{\link{phi.coef}}
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{eta.sq}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
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
  if (missing(x)) {

    stop("Please specify a vector, matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Vector, matrix or data frame for the argument 'x'?
  if (!is.vector(x) && !is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a vector, matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Check input 'group'
  if (missing(group)) {

    stop("Please specify a vector, matrix or data frame for the argument 'group'.", call. = FALSE)

  }

  #----------------------------------------
  # Data frame

  x <- as.data.frame(x)

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }


  #.........................................

  if (isTRUE(check)) {

    #......
    # Check input 'x': Is numeric?
    if (any(vapply(x, function(y) !is.numeric(y), FUN.VALUE = logical(1)))) {

      stop("Please specify a numeric vector, matrix or data frame with numeric vectors for the argument 'x'.",
           call. = FALSE)

    }

    #......
    # Check input 'x': Zero variance?
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1))

    if (any(x.zero.var)) {

      if (length(x.zero.var) > 1) {

        warning(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ",
                       paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

      } else {

        stop("Vector specified in 'x' has zero variance.", call. = FALSE)

      }

    }

    #......
    # Check input 'group': Is integer, character or factors?
    if (any(vapply(as.data.frame(group), function(y) any(as.numeric(y) %% 1 != 0, na.rm = TRUE), FUN.VALUE = logical(1)))) {

      stop("Please specify a integer vector, matrix or data frame with integer vectors, character vectors or factors for the argument 'x'",
           call. = FALSE)

    }

    #......
    # Check input 'group': At least two groups?
    group.check <- vapply(as.data.frame(group), function(y) length(na.omit(unique(y))) < 2, FUN.VALUE = logical(1))

    if (any(group.check)) {

      if (length(group.check) > 1) {

        stop(paste0("Follwing grouping variables specified in 'group' do not have at least two groups: ",
                    paste(names(which(group.check)), collapse = ", ")), call. = FALSE)

      } else {

        stop("Grouping variable specified in 'group' does not have at least two groups.", call. = FALSE)

      }

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 || digits < 0) {

      warning("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) || isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #-----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x <- misty::as.na(x, as.na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(x.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    # Constant variables
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1))
    if (any(x.zero.var)) {

      stop(paste0("After converting user-missing values into NA, following variables have only one unique value: ",
                  paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

  }

  #-----------------------------------------
  # Number of dependent variables, number of independent variables

  x.ncol <- ncol(x) == 1

  group.dim.null <- is.null(dim(group))

  ####################################################################################
  # Main Function

  #----------------------------------------
  # One dependent variable, one independent variable

  if (x.ncol && group.dim.null) {

      # Estimate model
      mod <- aov(x ~ factor(group), data = data.frame(x, group))

      # Model summary
      mod.summary <- summary(mod)

      # Mean Squared Error Between
      SQ.B <- unlist(mod.summary[[1]])["Sum Sq1"]

      # Mean Squared Error Within
      SQ.T <- SQ.B + unlist(mod.summary[[1]])["Sum Sq2"]

      # Eta squared
      eta <- unname(SQ.B / SQ.T)

      # NaN to NA
      eta <- ifelse(is.nan(eta), NA, eta)

  }

  #----------------------------------------
  # More than one dependent variable, one independent variable

  if (!x.ncol && group.dim.null) {

    eta <- matrix(NA, ncol = ncol(x), dimnames = list(NULL, colnames(x)))
    for (i in seq_len(ncol(x))) {

      eta[, i] <- misty::eta.sq(x[, i], group = group, check = FALSE, output = FALSE)$result

    }

  }

  #----------------------------------------
  # One dependent variable, more than one independent variable

  if (x.ncol && !group.dim.null) {

    eta <-  matrix(NA, nrow = ncol(group), dimnames = list(colnames(group), NULL))
    for (i in seq_len(ncol(group))) {

      eta[i, ] <- misty::eta.sq(x, group[, i], check = FALSE, output = FALSE)$result

    }

  }

  #----------------------------------------
  # More than one dependent variable, more than one independent variable

  if (!x.ncol && !group.dim.null) {

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
                 dat = list(x = x, group = group),
                 args = list(digits = digits, as.na = as.na, check = check, output = output),
                 result = eta)

  class(object) <- "eta.sq"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
