#' Phi Coefficient
#'
#' This function computes the (adjusted) Phi coefficient between two or more than two dichotomous variables.
#'
#' The maximum Phi coefficient is determined by the distribution of the two variables, i.e., the Phi
#' coefficient cannot achieve the value of 1 in many cases. According to Cureton (1959), the' phi
#' coefficient can be adjusted by relating the coefficient to the possible maximum, \eqn{\phi / \phi_max}.
#'
#' @param x           a matrix or data frame.
#' @param adjust      logical: if \code{TRUE}, phi coefficient is adjusted by relating
#'                    the coefficient to the possible maximum.
#' @param tri         a character string or character vector indicating which triangular of the matrix
#'                    to show on the console, i.e., \code{both} for upper and lower triangular,
#'                    \code{lower} (default) for the lower triangular, and \code{upper} for the upper
#'                    triangular.
#' @param digits      an integer value indicating the number of decimal places digits to be used for
#'                    displaying phi coefficients.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @references
#' Cureton, E. E. (1959). Note on Phi/Phi max. \emph{Psychometrika, 24}, 89-91.
#'
#' Davenport, E. C., & El-Sanhurry, N. A. (1991). Phi/Phimax: Review and synthesis. \emph{Educational and
#' Psychological Measurement, 51}, 821-828. https://doi.org/10.1177/001316449105100403
#'
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' New York: John Wiley & Sons.
#'
#' @return
#' Returns an object of class \code{phi.coef}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(0, 1, 0, 1, 0, 1, 0, 1, 1, 0),
#'                   x2 = c(0, 1, 0, 0, 1, 1, 1, 1, 1, 1),
#'                   x3 = c(0, 1, 0, 1, 1, 1, 1, 1, 0, 0))
#'
#' # Phi coefficient between x1 and x2
#' phi.coef(dat[, c("x1", "x2")])
#'
#' # Adjusted phi coefficient between x1 and x2
#' phi.coef(dat[, c("x1", "x2")], adjust = TRUE)
#'
#' # Phi coefficient matrix between x1, x2, and x3
#' phi.coef(dat)
#'
#' # Adjusted phi coefficient matrix between x1, x2, and x3
#' phi.coef(dat, adjust = TRUE)
phi.coef <- function(x, adjust = FALSE, tri = c("both", "lower", "upper"),
                     digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (missing(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #......
  # Matrix or data frame for the argument 'x'?
  if (!is.matrix(x) && !is.data.frame(x)) {

    stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE)

  }

  #-----------------------------------------
  # As data frame

  x <- as.data.frame(x)

  #-----------------------------------------
  # Convert user-missing values into NA

  if (!is.null(as.na)) {

    x <- misty::as.na(x, as.na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1))
    if (any(x.miss)) {

      stop(paste0("After converting user-mising values into NA, following variables are completely missing: ",
                  paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    # Constant variables
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1))
    if (any(x.zero.var)) {

      stop(paste0("After converting user-mising values into NA, following variables have only one unique value: ",
                  paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #------------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x'
    if (any(vapply(x, function(y) any(as.numeric(y) %% 1 != 0, na.rm = TRUE), FUN.VALUE = logical(1)))) {

      stop("Please specify a matrix or data frame with integer vectors for the argument 'x'.", call. = FALSE)

    }

    #......
    # Check input 'x': Zero variance
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1))
    if (any(x.zero.var)) {

      if (length(x.zero.var) == 2) {

        stop(paste0("Following variables in the matrix or data frame specified in 'x' have only one unique value: ",
                   paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

      } else {

        warning(paste0("Following variables in the matrix or data frame specified in 'x' have only one unique value: ",
                      paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)
      }

    }

    #......
    # Check input 'x'
    if (any(vapply(x, function(y) length(na.omit(unique(y))) > 2, FUN.VALUE = logical(1)))) {

      stop("Please specify a matrix or data frame with dichotomous variables for the argument 'x'.",
           call. = FALSE)

    }

    #......
    # Check input 'adjust'
    if (isFALSE(isTRUE(adjust) || isFALSE(adjust))) {

      stop("Please specify TRUE or FALSE for the argument 'adjust'.", call. = FALSE)

    }

    #......
    # Check input 'tri'
    if (any(!tri %in% c("both", "lower", "upper"))) {

      stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".",
           call. = FALSE)

    }

    #......
    # Check input 'digits'
    if (digits %% 1 != 0 || digits < 0) {

      warning("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) || isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }

  }

  ####################################################################################
  # Data and Arguments

  #----------------------------------------
  # Print triangular

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  ####################################################################################
  # Main Function

  #----------------------------------------
  # Two variables

  if (ncol(x) == 2) {

    tab <- table(x)

    #.................
    # If two dichotomous variables
    if (nrow(tab) == 2 && ncol(tab) == 2) {

      # As numeric to avoid integer overflow
      a <- as.numeric(tab[1, 1])
      b <- as.numeric(tab[1, 2])
      c <- as.numeric(tab[2, 1])
      d <- as.numeric(tab[2, 2])

      # Phi coefficient
      phi <- (a*d - b*c) / (sqrt( (a + c) * (b + d) * (a + b) * (c + d) ))

      # Adjusted phi coefficient
      if (isTRUE(adjust)) {

        if (phi > 0) {

          phi.max <- min(c(sqrt((sum(tab[1, ])*sum(tab[, 2])) / (sum(tab[, 1])*sum(tab[2, ]))),
                           sqrt((sum(tab[, 1])*sum(tab[2, ])) / (sum(tab[1, ])*sum(tab[, 2])))))

          phi <- phi / phi.max

        } else {

          phi.max <- max(c(-sqrt((sum(tab[1, ])*sum(tab[, 1])) / (sum(tab[2, ])*sum(tab[, 2]))),
                           -sqrt((sum(tab[2, ])*sum(tab[, 2])) / (sum(tab[1, ])*sum(tab[, 1])))))

          phi <- -(phi / phi.max)

        }

      }

    #.................
    # If not two dichotomous variables
    } else {

      phi <- NA

    }

  #----------------------------------------
  # More than two variables
  } else {

    #......
    # Pairwise combination
    comb.n <- combn(ncol(x), m = 2)

    #......
    # Compute all pairwise contingency coefficients
    comb.n.phi <- rep(NA, times = ncol(comb.n))
    for (i in seq_len(ncol(comb.n))) {

      comb.n.phi[i] <- misty::phi.coef(x[, comb.n[, i]], adjust = adjust, as.na = as.na, check = FALSE, output = FALSE)$result

    }

    #......
    # Contingency coefficient matrix
    phi <- matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(colnames(x), colnames(x)))

    #......
    # Assign contingenfy coefficients to lower triangular
    phi[lower.tri(phi)] <- comb.n.phi

    #......
    # Copy lower triangular to upper triangular
    phi[upper.tri(phi)] <- t(phi)[upper.tri(phi)]

    #......
    # Set diagonal to 1
    diag(phi) <- 1

  }

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 data = x,
                 args = list(adjust = adjust, tri = tri, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = phi)

  class(object) <- "phi.coef"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
