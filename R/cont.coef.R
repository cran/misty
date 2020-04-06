#' Pearson's Contingency Coefficient
#'
#' This function computes the (adjusted) Pearson's contingency coefficient between two or more than two variables.
#'
#' The maximum contingency coefficient is determined by the distribution of the two variables, i.e., the
#' contingency coefficient cannot achieve the value of 1 in many cases. According to Sakoda (1977), the
#' contingency coefficient can be adjusted by relating the coefficient to the possible maximum, \eqn{C / C_max}.
#'
#' @param x           a matrix or data frame with integer vectors, character vectors or factors..
#' @param adjust      logical: if \code{TRUE}, the adjusted contingency coefficient (i.e., Sakoda's
#'                    adjusted Pearson's C) is computed.
#' @param tri         a character string indicating which triangular of the matrix to show on the console,
#'                    i.e., \code{both} for upper and lower triangular, \code{lower} (default) for the lower
#'                    triangular, and \code{upper} for the upper triangular.
#' @param digits      an integer value indicating the number of decimal places digits to be used for
#'                    displaying contingency coefficients.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' \code{\link{cohens.d}}, \code{\link{cor.matrix}}, \code{\link{cramers.v}}, \code{\link{phi.coef}}.
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' Sakoda, J.M. (1977). Measures of association for multivariate contingency tables. \emph{Proceedings of the
#' Social Statistics Section of the American Statistical Association (Part III)}, 777-780.
#'
#' @return
#' Returns an object of class \code{cont.coef}, which is a list with following entries: function call (\code{call}),
#' matrix or data frame specified in \code{x} (\code{data}), specification of function arguments (\code{args}), and
#' list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(1, 1, 2, 1, 3, 3, 2, 2, 1, 2),
#'                   y = c(3, 2, 3, 1, 2, 4, 1, 2, 3, 4),
#'                   z = c(2, 2, 2, 1, 2, 2, 1, 2, 1, 2), stringsAsFactors = FALSE)
#'
#' # Contingency coefficient between x and y
#' cont.coef(dat[, c("x", "y")])
#'
#' # Adjusted contingency coefficient between x and y
#' cont.coef(dat[, c("x", "y")], adjust = TRUE)
#'
#' # Contingency coefficient matrix between x, y, and z
#' cont.coef(dat)
#'
#' # Adjusted contingency coefficient matrix between x, y, and z
#' cont.coef(dat, adjust = TRUE)
cont.coef <- function(x, adjust = FALSE, tri = c("both", "lower", "upper"),
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

    stop("Please specifiy a matrix or data frame for the argument 'x'.",
         call. = FALSE)

  }

  #-----------------------------------------
  # As data frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

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

    # Zero variance
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))
    if (any(x.zero.var)) {

      stop(paste0("After converting user-missing values into NA, following variables have only one unique value: ",
                  paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

  }

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isFALSE(isTRUE(check) || isFALSE(check))) {

    stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE)

  }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # Check input 'x' for zero variance
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1L))
    if (any(x.zero.var)) {

      stop(paste0("Following variables in the matrix or data frame specified in 'x' have only one unique value: ",
                  paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

    #......
    # Check input 'x' for non-integer numbers
    if (any(vapply(x, function(y) any(as.numeric(y) %% 1 != 0, na.rm = TRUE), FUN.VALUE = logical(1L)))) {

      stop("Please specify a matrix or data frame with integer vectors, character vectors or factors for the argument 'x'.",
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
    if (digits %% 1L != 0L || digits < 0L) {

      stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE)

    }

    #......
    # Check input 'output'
    if (isFALSE(isTRUE(output) || isFALSE(output))) {

      stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE)

    }


  }

  ####################################################################################
  # Arguments

  #-----------------------------------------
  # Print triangular

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  ####################################################################################
  # Main Function

  #----------------------------------------
  # Two variables

  if (ncol(x) == 2L) {

    tab <- table(x)

    chisq <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)

    if (!is.nan(chisq)) {

      cc <- as.numeric(sqrt(chisq / (chisq + sum(tab))))

      # Sakoda's adjusted Pearson's C
      if (isTRUE(adjust)) {

        k <- min(c(nrow(tab), ncol(tab)))

        cc <- cc / sqrt((k - 1L) / k)

      }

    } else {

      cc <- NA
    }

  #----------------------------------------
  # More than two variables

  } else {

    # Pairwise combination
    comb.n <- combn(ncol(x), m = 2L)

    # Compute all pairwise contingency coefficients
    comb.n.cc <- rep(NA, times = ncol(comb.n))
    for (i in seq_len(ncol(comb.n))) {

      comb.n.cc[i] <- misty::cont.coef(x[, comb.n[, i]], adjust = adjust, as.na = as.na, check = FALSE, output = FALSE)$result

    }

    # Contingency coefficient matrix
    cc <- matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(colnames(x), colnames(x)))

    # Assign contingency coefficients to lower triangular
    cc[lower.tri(cc)] <- comb.n.cc

    # Copy lower triangular to upper triangular
    cc[upper.tri(cc)] <- t(cc)[upper.tri(cc)]

    # Set diagonal to 1
    diag(cc) <- 1L

  }

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "cont.coef",
                 data = x,
                 args = list(adjust = adjust, tri = tri, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = cc)

  class(object) <- "square.matrix"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
