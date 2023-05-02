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
#' \code{\link{cor.matrix}}, \code{\link{cor.cramer}}, \code{\link{cor.phi}}, \code{\link{cor.poly}}, \code{\link{cohens.d}}, .
#'
#' @references
#' Rasch, D., Kubinger, K. D., & Yanagida, T. (2011). \emph{Statistics in psychology - Using R and SPSS}.
#' John Wiley & Sons.
#'
#' Sakoda, J.M. (1977). Measures of association for multivariate contingency tables. \emph{Proceedings of the
#' Social Statistics Section of the American Statistical Association (Part III)}, 777-780.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries:
#' \tabular{ll}{
#' \code{call} \tab function call \cr
#' \code{type} \tab type of analysis \cr
#' \code{data} \tab matrix or data frame specified in \code{x}  \cr
#' \code{args} \tab specification of function arguments \cr
#' \code{result} \tab result table \cr
#' }
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x = c(1, 1, 2, 1, 3, 3, 2, 2, 1, 2),
#'                   y = c(3, 2, 3, 1, 2, 4, 1, 2, 3, 4),
#'                   z = c(2, 2, 2, 1, 2, 2, 1, 2, 1, 2))
#'
#' # Contingency coefficient between x and y
#' cor.cont(dat[, c("x", "y")])
#'
#' # Adjusted contingency coefficient between x and y
#' cor.cont(dat[, c("x", "y")], adjust = TRUE)
#'
#' # Contingency coefficient matrix between x, y, and z
#' cor.cont(dat)
#'
#' # Adjusted contingency coefficient matrix between x, y, and z
#' cor.cont(dat, adjust = TRUE)
cor.cont <- function(x, adjust = FALSE, tri = c("both", "lower", "upper"),
                     digits = 2, as.na = NULL, check = TRUE, output = TRUE) {

  #_____________________________________________________________________________
  #
  # Initial Check --------------------------------------------------------------

  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specifiy a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## As data frame ####

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert user-missing values into NA ####

  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

    # Variable with missing values only
    x.miss <- vapply(x, function(y) all(is.na(y)), FUN.VALUE = logical(1L))
    if (isTRUE(any(x.miss))) {

      stop(paste0("After converting user-missing values into NA, following variables are completely missing: ", paste(names(which(x.miss)), collapse = ", ")), call. = FALSE)

    }

    # Zero variance
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1L, FUN.VALUE = logical(1))
    if (isTRUE(any(x.zero.var))) {

      stop(paste0("After converting user-missing values into NA, following variables have only one unique value: ", paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE)

    }

  }

  #_____________________________________________________________________________
  #
  # Input Check ----------------------------------------------------------------

  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  if (isTRUE(check)) {

    # Check input 'x' for zero variance
    x.zero.var <- vapply(x, function(y) length(na.omit(unique(y))) == 1, FUN.VALUE = logical(1L))
    if (isTRUE(any(x.zero.var))) { stop(paste0("Following variables in the matrix or data frame specified in 'x' have only one unique value: ", paste(names(which(x.zero.var)), collapse = ", ")), call. = FALSE) }

    # Check input 'x' for non-integer numbers
    if (isTRUE(all(!vapply(x, is.character, FUN.VALUE = logical(1L))))) {

      if (isTRUE(any(vapply(x, function(y) any(as.numeric(y) %% 1 != 0, na.rm = TRUE), FUN.VALUE = logical(1L))))) { stop("Please specify a matrix or data frame with integer vectors, character vectors or factors for the argument 'x'.", call. = FALSE) }

    }

    # Check input 'adjust'
    if (isTRUE(!is.logical(adjust))) { stop("Please specify TRUE or FALSE for the argument 'adjust'.", call. = FALSE) }

    # Check input 'tri'
    if (isTRUE(any(!tri %in% c("both", "lower", "upper")))) { stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".", call. = FALSE) }

    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L || digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  #_____________________________________________________________________________
  #
  # Arguments ------------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Print triangular ####

  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  #_____________________________________________________________________________
  #
  # Main Function --------------------------------------------------------------

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Two variables ####

  if (isTRUE(ncol(x) == 2L)) {

    tab <- table(x)

    chisq <- suppressWarnings(chisq.test(tab, correct = FALSE)$statistic)

    if (isTRUE(!is.nan(chisq))) {

      cc <- as.numeric(sqrt(chisq / (chisq + sum(tab))))

      # Sakoda's adjusted Pearson's C
      if (isTRUE(adjust)) {

        k <- min(c(nrow(tab), ncol(tab)))

        cc <- cc / sqrt((k - 1L) / k)

      }

    } else {

      cc <- NA
    }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## More than two variables ####

  } else {

    # Pairwise combination
    comb.n <- combn(ncol(x), m = 2L)

    # Compute all pairwise contingency coefficients
    comb.n.cc <- rep(NA, times = ncol(comb.n))
    for (i in seq_len(ncol(comb.n))) {

      comb.n.cc[i] <- misty::cor.cont(x[, comb.n[, i]], adjust = adjust, as.na = as.na, check = FALSE, output = FALSE)$result

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

  #_____________________________________________________________________________
  #
  # Return Object --------------------------------------------------------------

  object <- list(call = match.call(),
                 type = "cor.cont",
                 data = x,
                 args = list(adjust = adjust, tri = tri, digits = digits, as.na = as.na,
                             check = check, output = output),
                 result = cc)

  class(object) <- "misty.object"

  #_____________________________________________________________________________
  #
  # Output ---------------------------------------------------------------------

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
