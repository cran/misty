#' Cramer's V
#'
#' This function computes the (bias-corrected) Cramer's V between two or more than two variables.
#'
#' Cramer's V can have large bias tending to overestimate the strength of association which depends
#' on the size of the table and the sample size. As proposed by Bergsma (2013) a bias correction can be
#' applied to obtain the bias-corrected Cramer's V.
#'
#' @param x           a matrix or data frame with integer vectors, character vectors or factors.
#' @param correct     logical: if \code{TRUE} (default), the bias-corrected Cramer's V is computed.
#' @param tri         a character string or character vector indicating which triangular of the matrix
#'                    to show on the console, i.e., \code{both} for upper and lower triangular,
#'                    \code{lower} (default) for the lower triangular, and \code{upper} for the upper
#'                    triangular.
#' @param digits      an integer value indicating the number of decimal places digits to be used for
#'                    displaying Cramer's V.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{cor.matrix}}, \code{\link{cor.cont}}, \code{\link{cor.phi}}, \code{\link{cor.poly}},
#' \code{\link{cohens.d}}.
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' Bergsma, W. (2013). A bias correction for Cramer's V and Tschuprow's T. \emph{Journal of the Korean
#' Statistical Society, 42}, 323-328. https://doi.org/10.1016/j.jkss.2012.10.002
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following entries:
#' function call (\code{call}), type of analysis \code{type}, matrix or data frame specified in
#' \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(1, 1, 2, 1, 3, 3, 2, 2, 1, 2),
#'                   y = c(1, 2, 2, 1, 3, 4, 1, 2, 3, 1),
#'                   z = c(1, 1, 2, 1, 2, 3, 1, 2, 3, 2))
#'
#' # Bias-corrected Cramer's V between x and y
#' cor.cramer(dat[, c("x", "y")])
#'
#' # Cramer's V between x and y
#' cor.cramer(dat[, c("x", "y")], correct = FALSE)
#'
#' # Bias-corrected Cramer's V matrix between x, y, and z
#' cor.cramer(dat[, c("x", "y", "z")])
#'
#' # Cramer's V matrix between x, y, and z
#' cor.cramer(dat[, c("x", "y", "z")], correct = FALSE)
cor.cramer <- function(x, correct = TRUE, tri = c("both", "lower", "upper"),
                      digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

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
  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) {

    stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #-----------------------------------------
  # Data frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #----------------------------------------
  # Convert user-missing values into NA

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    #......
    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    #......
    # Zero variance
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))
    if (isTRUE(any(x.zero.var))) {

      stop(paste0("After converting user-missing values into NA, following variables have only one unique value: ",
                  paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x'
    if (isTRUE(all(!vapply(x, is.character, FUN.VALUE = logical(1L))))) {

      if (isTRUE(any(vapply(x, function(y) any(as.numeric(y) %% 1L != 0L, na.rm = TRUE), FUN.VALUE = logical(1L))))) {

        stop("Please specify a matrix or data frame with integer vectors, character vectors or factors the argument 'x'.",
             call. = FALSE)

      }

    }

    #......
    # Check input 'x'
    if (isTRUE(ncol(x) == 1L)) {

      stop("Please specify a matrix or data frame with at least two variables for the argument 'x'.", call. = FALSE)

    }

    #......
    # Input 'x': Zero variance
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1L))
    if (isTRUE(any(x.zero.var))) {

      stop(paste0("Following variables in the matrix or data frame specified in 'x' have zero variance: ",
                  paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

    #......
    # Check input 'correct'
    if (isTRUE(!is.logical(correct))) {

      stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE)

    }

    #......
    # Check input 'tri'
    if (isTRUE(any(!tri %in% c("both", "lower", "upper")))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
              call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  #  Arguments

  #----------------------------------------
  # Print triangular

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  ####################################################################################
  # Main Function

  #----------------------------------------
  # Two variables

  if (isTRUE(ncol(x) == 2L)) {

    # Cross tabulation
    tab <- table(x)

    # Chi square
    chisq <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)

    # Number of columns
    k <- ncol(tab)

    # Number of rows
    r <- nrow(tab)

    # Sample size
    n <- sum(tab)

    # Bias correction
    if (isTRUE(correct)) {

      v <- sqrt(max(c(0, (chisq /  n) - ((k - 1)*(r - 1L)) / (n - 1))) / min(c((k - ((k - 1L)^2L / (n - 1L))) - 1, (r - ((r - 1L)^2L / (n - 1L))) - 1L)))

    } else {

      v <- as.numeric(sqrt(chisq /  n / min(c(r - 1L, k - 1L))))

    }

  #----------------------------------------
  # More than two variables
  } else {

    # Pairwise combination
    comb.n <- combn(ncol(x), m = 2L)

    # Compute all pairwise contingency coefficients
    comb.n.v <- rep(NA, times = ncol(comb.n))
    for (i in seq_len(ncol(comb.n))) {

      comb.n.v[i] <- misty::cor.cramer(x[, comb.n[, i]], correct = correct, as.na = as.na, check = FALSE, output = FALSE)$result

    }

    # Cramaer's V matrix
    v <- matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(colnames(x), colnames(x)))

    # Assign Cramer's V to lower triangular
    v[lower.tri(v)] <- comb.n.v

    # Copy lower triangular to upper triangular
    v[upper.tri(v)] <- t(v)[upper.tri(v)]

    # Set diagonal to 1
    diag(v) <- 1L

  }

  ####################################################################################
  # Return object

  # Return object
  object <- list(call = match.call(),
                 type = "cor.cramer",
                 data = x,
                 args = list(correct = correct, tri = tri, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = v)

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
