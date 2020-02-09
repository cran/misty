#' Cramer's V
#'
#' This function computes the (bias-corrected) Cramer's V between two or more than two variables.
#'
#' Cramer's V can have large bias tending to overestimate the strength of association which depends
#' on the size of the table and the sample size. As proposed by Bergsma (2013) a bias correction can be
#' applied to obatin the bias-corrected Cramer's V.
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
#' \code{\link{cohens.d}}, \code{\link{cont.coef}}, \code{\link{cor.matrix}}, \code{\link{phi.coef}}.
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' Bergsma, W. (2013). A bias correction for Cramer's V and Tschuprow's T. \emph{Journal of the Korean
#' Statistical Society, 42}, 323-328. https://doi.org/10.1016/j.jkss.2012.10.002
#'
#' @return
#' Returns an object of class \code{cramers.v}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
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
#' cramers.v(dat[, c("x", "y")])
#'
#' # Cramer's V between x and y
#' cramers.v(dat[, c("x", "y")], correct = FALSE)
#'
#' # Bias-corrected Cramer's V matrix between x, y, and z
#' cramers.v(dat[, c("x", "y", "z")])
#'
#' # Cramer's V matrix between x, y, and z
#' cramers.v(dat[, c("x", "y", "z")], correct = FALSE)
cramers.v <- function(x, correct = TRUE, tri = c("both", "lower", "upper"),
                      digits = 3, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #.........................................
  # Check input 'x'
  if (missing(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specifiy a matrix or data frame for the argument 'x'.",
         call. = FALSE)

  }

  #-----------------------------------------
  # Data frame

  x <- as.data.frame(x)

  #----------------------------------------
  # Convert user-missing values into NA
  if (!is.null(as.na)) {

    x <- misty::as.na(x, na = as.na, check = check)

    #......
    # Variable with missing values only
    x.miss <- sapply(x, function(y) all(is.na(y)))
    if (any(x.miss)) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    #......
    # Constant variables
    x.con <- sapply(x, function(y) var(as.numeric(y), na.rm = TRUE) == 0)
    if (any(x.con)) {

      stop(paste0("After converting user-missing values into NA, following variables are constant: ",
                  paste(names(which(x.con)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) | isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x'
    if (any(sapply(x, function(y) any(as.numeric(y) %% 1 != 0, na.rm = TRUE)))) {

      stop("Please specify a matrix or data frame with integer vectors, character vectors or factors the argument 'x'",
           call. = FALSE)

    }

    #......
    # Check input 'x'
    if (ncol(x) == 1) {

      stop("Please specify a matrix or data frame with at least two variables for the argument 'x'", call. = FALSE)

    }

    #......
    # Check input 'correct'
    if (isFALSE(isTRUE(correct) | isFALSE(correct))) {

      stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE)

    }

    #......
    # Check input 'tri'
    if (any(!tri %in% c("both", "lower", "upper"))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
              call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 | digits < 0) {

      stop("Specify a positive integer number for the argument 'digits'", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) | isFALSE(output))) {

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

  if (ncol(x) == 2) {

    # Cross tabulation
    tab <- table(x)

    # Chi square
    chisq <- suppressWarnings(chisq.test(tab, correct = TRUE)$statistic)

    # Number of columns
    k <- ncol(tab)

    # Number of rows
    r <- nrow(tab)

    # Sample size
    n <- sum(tab)

    # Bias correction
    if (isTRUE(correct)) {

      v <- sqrt(max(0, (chisq /  n) - ((k - 1)*(r - 1)) / (n - 1) ) / min( (k - ((k - 1)^2 / (n - 1))) - 1 , (r - ((r - 1)^2 / (n - 1))) - 1))

    } else {

      v <- as.numeric(sqrt(chisq /  n / min(r - 1, k - 1)))

    }

  #----------------------------------------
  # More than two variables
  } else {

    # Pairwise combination
    comb.n <- combn(ncol(x), m = 2)

    # Compute all pairwise contingency coefficients
    comb.n.v <- rep(NA, times = ncol(comb.n))
    for (i in 1:ncol(comb.n)) {

      comb.n.v[i] <- misty::cramers.v(x[, comb.n[, i]], correct = correct, as.na = as.na, check = FALSE, output = FALSE)$result

    }

    # Cramaer's V matrix
    v <- matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(colnames(x), colnames(x)))

    # Assign Cramer's V to lower triangular
    v[lower.tri(v)] <- comb.n.v

    # Copy lower triangular to upper triangular
    v[upper.tri(v)] <- t(v)[upper.tri(v)]

    # Set diagonal to 1
    diag(v) <- 1

  }

  ####################################################################################
  # Return object

  # Return object
  object <- list(call = match.call(),
                 data = x,
                 args = list(correct = correct, tri = tri, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = v)

  class(object) <- "cramers.v"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
