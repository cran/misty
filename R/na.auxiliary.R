#' Auxiliary variables analysis
#'
#' This function computes (1) Pearson product-moment correlation matrix to identify
#' variables related to the incomplete variable and (2) Cohen's d comparing cases
#' with and without missing values to identify variables related to the probability
#' of missingness.
#'
#' Note that non-numeric variables (i.e., factors, character vectors, and logical
#' vectors) are excluded from to the analysis.
#'
#' @param x           a matrix or data frame with numeric vectors.
#' @param tri         a character string indicating which triangular of the correlation
#'                    matrix to show on the console, i.e., \code{both} for upper and
#'                    lower triangular, \code{lower} (default) for the lower triangular,
#'                    and \code{upper} for the upper triangular.
#' @param weighted    logical: if \code{TRUE} (default), the weighted pooled standard
#'                    deviation is used.
#' @param correct     logical: if \code{TRUE}, correction factor for Cohen's d to
#'                    remove positive bias in small samples is used.
#' @param digits      integer value indicating the number of decimal places digits
#'                    to be used for displaying correlation coefficients and Cohen's d
#'                    estimates.
#' @param as.na       a numeric vector indicating user-defined missing values,
#'                    i.e. these values are converted to \code{NA} before conducting
#'                    the analysis.
#' @param check       logical: if \code{TRUE}, argument specification is checked.
#' @param output      logical: if \code{TRUE}, output is shown on the console.
#'
#' @author
#' Takuya Yanagida \email{takuya.yanagida@@univie.ac.at}
#'
#' @seealso
#' \code{\link{as.na}}, \code{\link{na.as}}, \code{\link{na.coverage}},
#' \code{\link{na.descript}}, \code{\link{na.indicator}}, \code{\link{na.pattern}},
#' \code{\link{na.prop}}, \code{\link{na.test}}
#'
#' @references
#' Enders, C. K. (2010). \emph{Applied missing data analysis}. Guilford Press.
#'
#' Graham, J. W. (2009). Missing data analysis: Making it work in the real world.
#' \emph{Annual Review of Psychology, 60}, 549-576.
#' https://doi.org/10.1146/annurev.psych.58.110405.085530
#'
#' van Buuren, S. (2018). \emph{Flexible imputation of missing data} (2nd ed.).
#' Chapman & Hall.
#'
#' @return
#' Returns an object of class \code{misty.object}, which is a list with following
#' entries: function call (\code{call}), type of analysis \code{type}, matrix or
#' data frame specified in \code{x} (\code{data}), specification of function arguments
#' (\code{args}), and list with results (\code{result}).
#'
#' @export
#'
#' @examples
#' dat <- data.frame(x1 = c(1, NA, 2, 5, 3, NA, 5, 2),
#'                   x2 = c(4, 2, 5, 1, 5, 3, 4, 5),
#'                   x3 = c(NA, 3, 2, 4, 5, 6, NA, 2),
#'                   x4 = c(5, 6, 3, NA, NA, 4, 6, NA))
#'
#' # Auxiliary variables
#' na.auxiliary(dat)
na.auxiliary <- function(x, tri = c("both", "lower", "upper"), weighted = TRUE,
                         correct = FALSE, digits = 2, as.na = NULL, check = TRUE,
                         output = TRUE) {


  ####################################################################################
  # Function

  cohens.d.na.auxiliary <- function(formula, data, weighted = TRUE, correct = FALSE) {

    #-----------------------------------------------------------------------------------
    # Formula

    # Variables
    var.formula <- all.vars(as.formula(formula))

    # Outcome(s)
    y.var <- var.formula[-length(var.formula)]

    # Grouping variable
    group.var <- var.formula[length(var.formula)]

    # Data
    data <- as.data.frame(data[, var.formula], stringsAsFactors = FALSE)

    #...................
    # Data and Arguments

    # Outcome
    x.dat <- data[, y.var]

    # Grouping
    group.dat <- data[, group.var]

    #...................
    # Descriptives

    # Mean difference
    x.diff <- diff(tapply(x.dat, group.dat, mean, na.rm = TRUE))

    # Sample size by group
    n.group <- tapply(x.dat, group.dat, function(y) length(na.omit(y)))

    #...................
    # Standard deviation

    # Variance by group
    var.group <- tapply(x.dat, group.dat, var, na.rm = TRUE)

    # Weighted pooled standard deviation
    if (isTRUE(weighted)) {

    sd.group <- sqrt(((n.group[1L] - 1L)*var.group[1] + (n.group[2L] - 1L)*var.group[2L]) / (sum(n.group) - 2L))

    # Unweigted pooled standard deviation
    } else {

      sd.group <- sum(var.group) / 2L

    }

    #........................................
    # Cohen's d estimate

    estimate <- x.diff / sd.group

    #........................................
    # Correction factor

    # Bias-corrected Cohen's d
    if (isTRUE(correct && weighted)) {

      v <- sum(n.group) - 2L

      # Correction factor based on gamma function
      if (isTRUE(sum(n.group) < 200L)) {

        corr.factor <- gamma(0.5*v) / ((sqrt(v / 2)) * gamma(0.5 * (v - 1L)))

      # Correction factor based on approximation
      } else {

        corr.factor <- (1L - (3L / (4L * v - 1L)))

      }

      estimate <- estimate*corr.factor

    }

    return(estimate)

  }

  ####################################################################################
  # Data

  #......
  # Check if input 'x' is missing
  if (isTRUE(missing(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #......
  # Check if input 'x' is NULL
  if (isTRUE(is.null(x))) { stop("Input specified for the argument 'x' is NULL.", call. = FALSE) }

  #......
  # Matrix or data frame for the argument 'x'?
  if (isTRUE(!is.matrix(x) && !is.data.frame(x))) { stop("Please specify a matrix or data frame for the argument 'x'.", call. = FALSE) }

  #----------------------------------------
  # Data frame

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  #-----------------------------------------
  # Exclude factors, character vectors and logical vectors

  x.num <- vapply(x, function(y) is.numeric(y), FUN.VALUE = logical(1))

  if (isTRUE(any(!x.num))) {


    # Select numeric variables
    x <- x[, which(x.num)]

    warning(paste0("Non-numeric variables excluded from the analysis: ",
                   paste(names(x.num)[!x.num], collapse = ", ")), call. = FALSE)


  }

  #-----------------------------------------
  # Convert user-missing values into NA
  if (isTRUE(!is.null(as.na))) {

    x <- misty::as.na(x, na = as.na, check = check)

  }

  ####################################################################################
  # Input Check

  #......
  # Check input 'check'
  if (isTRUE(!is.logical(check))) { stop("Please specify TRUE or FALSE for the argument 'check'.", call. = FALSE) }

  #-----------------------------------------

  if (isTRUE(check)) {

    #......
    # No missing values
    if (isTRUE(all(!is.na(x)))) { stop("There are no missing values (NA) in the matrix or data frame specified in 'x'.", call. = FALSE) }

    #......
    # Check input 'tri'
    if (isTRUE(any(!tri %in% c("both", "lower", "upper")))) { stop("Character string in the argument 'tri' does not match with \"both\", \"lower\", or \"upper\".", call. = FALSE)

    }

    #......
    # Check input 'weighted'
    if (isTRUE(!is.logical(weighted))) { stop("Please specify TRUE or FALSE for the argument 'weighted'.", call. = FALSE) }

    #......
    # Check input 'correct'
    if (isTRUE(!is.logical(correct))) { stop("Please specify TRUE or FALSE for the argument 'correct'.", call. = FALSE) }

    #......
    # Check input 'digits'
    if (isTRUE(digits %% 1L != 0L | digits < 0L)) { stop("Specify a positive integer number for the argument 'digits'.", call. = FALSE) }

    #......
    # Check input 'output'
    if (isTRUE(!is.logical(output))) { stop("Please specify TRUE or FALSE for the argument 'output'.", call. = FALSE) }

  }

  ####################################################################################
  # Arguments

  #-----------------------------------------
  # Print triangular
  tri <- ifelse(all(c("both", "lower", "upper") %in% tri), "lower", tri)

  ####################################################################################
  # Main Function

  #-----------------------------------------
  # Variables related to the incomplete variable

  cor.mat <- cor(x, use = "pairwise.complete.obs")

  #-----------------------------------------
  # Variables related to the probability of missingness

  #......
  # Indicator matrix
  ind <- misty::na.indicator(x)
  colnames(ind) <- paste0(colnames(ind), "_ind")

  #......
  # Pairwise combinations
  x.combn <- combn(ncol(x), m = 2L)

  #......
  # Data
  x.ind <- data.frame(x, ind, stringsAsFactors = FALSE)

  #......
  # Cohen's d

  result.d.upp <- numeric(ncol(x.combn))
  result.d.low <- numeric(ncol(x.combn))
  for (i in seq_len(ncol(x.combn))) {

    temp <- x.combn[, i]

    if (isTRUE(length(unique(x.ind[, colnames(ind)[temp[1L]]])) == 2L &&
        all(tapply(x.ind[,  names(x)[temp[2L]]], x.ind[colnames(ind)[temp[1]]], function(y) length(unique(na.omit(y)))) > 0L))) {

      result.d.upp[i] <- eval(parse(text = paste0("cohens.d.na.auxiliary(", names(x)[temp[2L]], " ~ ", colnames(ind)[temp[1L]],
                                                  ", data = x.ind, weighted = weighted, correct = correct)")))

    } else {

      result.d.upp[i] <- NA

    }

    if (isTRUE(length(unique(x.ind[, colnames(ind)[temp[2L]]])) == 2L &&
        all(tapply(x.ind[,  names(x)[temp[1L]]], x.ind[colnames(ind)[temp[2]]], function(y) length(unique(na.omit(y)))) > 0L))) {

      result.d.low[i] <- eval(parse(text = paste0("cohens.d.na.auxiliary(", names(x)[temp[1L]], " ~ ", colnames(ind)[temp[2L]],
                                                  ", data = x.ind, weighted = weighted, correct = correct)")))

    } else {

      result.d.low[i] <- NA

    }

  }

  #-----------------------------------------
  # Cohen's d matrix

  d.mat <-  matrix(NA, ncol = ncol(x), nrow = ncol(x), dimnames = list(names(x), names(x)))

  d.mat[rev(upper.tri(d.mat))] <- result.d.upp
  d.mat <- t(d.mat)

  d.mat[lower.tri(d.mat)] <- result.d.low

  ####################################################################################
  # Return object

  object <- list(call = match.call(),
                 type = "na.auxiliary",
                 data = list(x = x, x = x),
                 args = list(tri = tri, weighted = weighted, correct = correct, digits = digits,
                             as.na = as.na, check = check, output = output),
                 result = list(cor = cor.mat, d = d.mat))

  class(object) <- "misty.object"

  ####################################################################################
  # Output

  if (isTRUE(output)) { print(object, check = FALSE) }

  return(invisible(object))

}
